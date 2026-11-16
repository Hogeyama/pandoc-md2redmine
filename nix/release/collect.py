#!/usr/bin/env python3
"""Assemble the license notices and corresponding sources of a release.

Input is a JSON description of every component linked into the distributed
binary (see default.nix). The output directory receives:

  licenses/    notices to ship next to the binary
  sources/     source archives and the patches nixpkgs applied to them; a
               source that nixpkgs keeps unpacked is archived here
  components.json
               one record per component with its license and source hashes

The build fails when a component has no notice file, or when the binary
loads a shared library that no listed native component provides.
"""

from __future__ import annotations

import gzip
import hashlib
import json
import re
import shutil
import subprocess
import sys
import tarfile
from pathlib import Path, PurePosixPath

NOTICE_RE = re.compile(r"^(licen[cs]e|copying|copyright|notice|authors)", re.I)
CABAL_LICENSE_RE = re.compile(r"^\s*license-files?\s*:\s*(.*)$", re.I)


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def strip_root(name: str) -> PurePosixPath | None:
    """Return the member path below the archive's top directory."""
    parts = PurePosixPath(name).parts
    if len(parts) < 2 or parts[0] in ("/", "..") or ".." in parts:
        return None
    return PurePosixPath(*parts[1:])


def matches(rel: PurePosixPath, patterns: list[PurePosixPath]) -> bool:
    return any(len(rel.parts) == len(p.parts) and rel.match(str(p)) for p in patterns)


def read_members(
    src: Path, patterns: list[PurePosixPath]
) -> dict[PurePosixPath, bytes]:
    """Read the regular files that could carry notices or notice metadata.

    The source is an archive with a top directory, or an unpacked directory.
    """

    def wanted(rel: PurePosixPath) -> bool:
        shallow = len(rel.parts) <= 3
        return shallow or rel.suffix in (".cabal", ".xml") or matches(rel, patterns)

    found: dict[PurePosixPath, bytes] = {}
    if src.is_dir():
        for path in sorted(src.rglob("*")):
            rel = PurePosixPath(path.relative_to(src).as_posix())
            if path.is_file() and not path.is_symlink() and wanted(rel):
                found[rel] = path.read_bytes()
        return found
    with tarfile.open(src, "r:*") as tar:
        for m in tar:
            # Archives store identical files as hard links to the first copy.
            if not (m.isfile() or m.islnk()):
                continue
            rel = strip_root(m.name)
            if rel is None or not wanted(rel):
                continue
            f = tar.extractfile(m)
            if f is not None:
                found[rel] = f.read()
    return found


def pack_directory(src: Path, dest: Path, top: str) -> None:
    """Archive a source directory reproducibly, under a top directory."""
    with dest.open("wb") as raw, gzip.GzipFile(
        filename="", mode="wb", fileobj=raw, mtime=0
    ) as gz, tarfile.open(fileobj=gz, mode="w", format=tarfile.PAX_FORMAT) as tar:
        for path in sorted(src.rglob("*")):
            info = tar.gettarinfo(str(path), arcname=f"{top}/{path.relative_to(src).as_posix()}")
            info.mtime = 1
            info.uid = info.gid = 0
            info.uname = info.gname = ""
            if info.isfile():
                with path.open("rb") as f:
                    tar.addfile(info, f)
            else:
                tar.addfile(info)


def cabal_license_files(members: dict[PurePosixPath, bytes]) -> set[PurePosixPath]:
    wanted: set[PurePosixPath] = set()
    for rel, data in members.items():
        if len(rel.parts) != 1 or rel.suffix != ".cabal":
            continue
        lines = data.decode("utf-8", "replace").splitlines()
        for i, line in enumerate(lines):
            m = CABAL_LICENSE_RE.match(line)
            if not m:
                continue
            values = [m.group(1)]
            # Continuation lines are indented deeper than the field name.
            indent = len(line) - len(line.lstrip())
            for nxt in lines[i + 1 :]:
                if nxt.strip() and len(nxt) - len(nxt.lstrip()) > indent:
                    values.append(nxt)
                else:
                    break
            for v in values:
                for name in re.split(r"[,\s]+", v.strip()):
                    if name:
                        wanted.add(PurePosixPath(name))
    return wanted


def select_notices(
    members: dict[PurePosixPath, bytes], patterns: list[PurePosixPath]
) -> dict[PurePosixPath, bytes]:
    wanted = cabal_license_files(members)
    selected = {}
    for rel, data in members.items():
        top_level_notice = len(rel.parts) == 1 and NOTICE_RE.match(rel.name)
        if top_level_notice or rel in wanted or matches(rel, patterns):
            selected[rel] = data
    return selected


def skylighting_syntax_notice(members: dict[PurePosixPath, bytes]) -> str:
    """List the KDE syntax definitions compiled into skylighting."""
    attr = lambda tag, key: (re.search(rf'\b{key}="([^"]*)"', tag) or [None, ""])[1]
    rows = []
    for rel, data in sorted(members.items()):
        if rel.parts[0] != "xml" or rel.suffix != ".xml":
            continue
        text = data.decode("utf-8", "replace")
        tag = re.search(r"<language\b[^>]*>", text, re.S)
        if tag is None:
            continue
        t = tag.group(0)
        rows.append(
            f"{rel.name}\n  name:    {attr(t, 'name')}\n"
            f"  author:  {attr(t, 'author')}\n  license: {attr(t, 'license')}\n"
        )
    header = (
        "Syntax definitions from the KDE syntax-highlighting framework, as\n"
        "bundled by skylighting-core and compiled into skylighting. Each entry\n"
        "gives the author and license declared in the definition file. The\n"
        "files themselves are in the skylighting-core source archive.\n\n"
    )
    return header + "\n".join(rows)


def citeproc_locale_notice(members: dict[PurePosixPath, bytes]) -> str:
    """List the CSL locale files compiled into citeproc with their rights."""
    rows = []
    for rel, data in sorted(members.items()):
        if rel.parts[0] != "locales" or rel.suffix != ".xml":
            continue
        text = data.decode("utf-8", "replace")
        rights = re.search(r"<rights\b([^>]*)>(.*?)</rights>", text, re.S)
        names = re.findall(r"<translator>.*?<name>(.*?)</name>", text, re.S)
        lic = ""
        if rights:
            url = re.search(r'license="([^"]*)"', rights.group(1))
            lic = f"{rights.group(2).strip()} ({url.group(1) if url else ''})"
        rows.append(
            f"{rel.name}\n  rights:      {lic}\n"
            f"  translators: {', '.join(n.strip() for n in names)}\n"
        )
    header = (
        "Citation Style Language locale files compiled into citeproc.\n"
        "Source: https://github.com/citation-style-language/locales\n\n"
    )
    return header + "\n".join(rows)


SPECIAL_NOTICES = {
    "skylighting-core": ("SYNTAX-DEFINITIONS.txt", skylighting_syntax_notice),
    "citeproc": ("CSL-LOCALES.txt", citeproc_locale_notice),
}


def collect(component: dict, out: Path) -> dict:
    kind, cid = component["kind"], f'{component["name"]}-{component["version"]}'
    src = Path(component["src"])
    patterns = [PurePosixPath(e) for e in component.get("extraNotices", [])]
    members = read_members(src, patterns)
    notices = select_notices(members, patterns)
    if not notices:
        sys.exit(f"error: no notice file found in {src} for {cid}")

    lic_dir = out / "licenses" / kind / cid
    for rel, data in notices.items():
        dest = lic_dir / rel
        dest.parent.mkdir(parents=True, exist_ok=True)
        dest.write_bytes(data)
    special = SPECIAL_NOTICES.get(component["name"])
    if special:
        name, render = special
        (lic_dir / name).write_text(render(members))

    src_dir = out / "sources" / kind
    src_dir.mkdir(parents=True, exist_ok=True)
    src_name = component.get("srcName", src.name.split("-", 1)[-1])
    src_dest = src_dir / (f"{src_name}.tar.gz" if src.is_dir() else src_name)
    if src_dest.exists():
        sys.exit(f"error: {cid} would overwrite {src_dest.relative_to(out)}; give it a srcName")
    if src.is_dir():
        pack_directory(src, src_dest, src_name)
    else:
        shutil.copyfile(src, src_dest)
    patches = []
    for p in component.get("patches", []):
        pdir = src_dir / f"{cid}-patches"
        pdir.mkdir(exist_ok=True)
        pdest = pdir / Path(p).name.split("-", 1)[-1]
        shutil.copyfile(p, pdest)
        patches.append({"path": str(pdest.relative_to(out)), "sha256": sha256(pdest)})

    return {
        "kind": kind,
        "name": component["name"],
        "version": component["version"],
        "license": component["license"],
        "homepage": component.get("homepage", ""),
        "notices": sorted(str((lic_dir / r).relative_to(out)) for r in notices),
        "source": {"path": str(src_dest.relative_to(out)), "sha256": sha256(src_dest)},
        "patches": patches,
    }


def check_shared_libraries(binary: str, ldd: str, native: list[dict]) -> None:
    """Fail when the binary loads a library outside the listed components."""
    output = subprocess.run([ldd, binary], check=True, capture_output=True, text=True).stdout
    prefixes = [p for c in native for p in c["outPaths"]]
    loaded = re.findall(r"(/nix/store/[^\s)]+)", output)
    unknown = [lib for lib in loaded if not any(lib.startswith(p + "/") for p in prefixes)]
    if unknown:
        sys.exit("error: shared libraries not covered by a native component:\n  " + "\n  ".join(unknown))
    unused = [c["name"] for c in native if not any(lib.startswith(p + "/") for lib in loaded for p in c["outPaths"])]
    if unused:
        sys.exit(f"error: native components the binary does not load: {unused}")


def main() -> None:
    spec = json.loads(Path(sys.argv[1]).read_text())
    out = Path(sys.argv[2])
    check_shared_libraries(spec["binary"], spec["ldd"], spec["native"])
    records = [collect(c, out) for c in spec["components"]]
    (out / "components.json").write_text(json.dumps(records, indent=2, ensure_ascii=False) + "\n")


if __name__ == "__main__":
    main()
