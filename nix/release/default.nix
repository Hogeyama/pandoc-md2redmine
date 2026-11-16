# The publication set of a GitHub release: the bundled executable together
# with the license notices and corresponding sources of everything in it.
#
# The executable statically links GHC's runtime and boot libraries and every
# Haskell library it depends on, and the bundle carries the shared libraries
# the executable loads. Each of those is a component below.
{ pkgs
, package
, bundled
, src
}:

let
  inherit (pkgs) lib;
  ghc = package.compiler;

  licenseOf = drv:
    let l = drv.meta.license or [ ];
    in map (x: x.spdxId or x.shortName or "unknown") (lib.toList l);

  storePath = p: "${p}";

  # Library and executable dependencies only: test and build-tool
  # dependencies are not linked. GHC's boot libraries appear as null here
  # and are covered by the GHC component.
  linkedDeps = drv:
    builtins.filter (d: d != null && d ? isHaskellLibrary)
      (drv.getCabalDeps.libraryHaskellDepends
        ++ drv.getCabalDeps.executableHaskellDepends);

  haskellClosure = map (e: e.drv) (builtins.genericClosure {
    startSet = map (d: { key = d.pname; drv = d; }) (linkedDeps package);
    operator = e: map (d: { key = d.pname; drv = d; }) (linkedDeps e.drv);
  });

  haskellComponent = drv: {
    kind = "haskell";
    name = drv.pname;
    inherit (drv) version;
    license = licenseOf drv;
    homepage = "https://hackage.haskell.org/package/${drv.pname}-${drv.version}";
    src = storePath drv.src;
    patches = map storePath (drv.patches or [ ]);
  };

  # The shared libraries the executable loads. collect.py fails the build if
  # this list and the libraries ldd reports disagree.
  native = [
    { drv = pkgs.glibc; extraNotices = [ ]; }
    { drv = pkgs.gmp; extraNotices = [ ]; }
    { drv = pkgs.libffi; extraNotices = [ ]; }
    # zlib has no LICENSE file; README carries the same license text as zlib.h.
    { drv = pkgs.zlib; extraNotices = [ "README" ]; }
  ];

  nativeComponent = { drv, extraNotices }: {
    kind = "native";
    name = drv.pname;
    inherit (drv) version;
    license = licenseOf drv;
    homepage = drv.meta.homepage or "";
    src = storePath drv.src;
    patches = map storePath (drv.patches or [ ]);
    inherit extraNotices;
    outPaths = [ (storePath drv.out) ];
  };

  ghcComponent = {
    kind = "ghc";
    name = "ghc";
    inherit (ghc) version;
    license = licenseOf ghc;
    homepage = "https://www.haskell.org/ghc/";
    src = storePath ghc.src;
    patches = map storePath (ghc.patches or [ ]);
    # The runtime system is under the top-level LICENSE, except for the ELF
    # relocation definitions its linker takes from LLVM. Each boot library
    # carries its own LICENSE; containers keeps it one level deeper.
    extraNotices = [
      "rts/linker/ELFRelocs/LICENSE-LLVM.TXT"
      "libraries/*/LICENSE"
      "libraries/containers/containers/LICENSE"
    ];
  };

  selfSource = pkgs.runCommand "${package.pname}-${package.version}-src.tar.gz"
    { nativeBuildInputs = [ pkgs.gnutar pkgs.gzip ]; } ''
    tar --sort=name --mtime=@1 --owner=0 --group=0 --numeric-owner \
      --transform 's,^\.,${package.pname}-${package.version},' \
      -C ${src} -c . | gzip -n > $out
  '';

  selfComponent = {
    kind = "self";
    name = package.pname;
    inherit (package) version;
    license = licenseOf package;
    homepage = package.meta.homepage or "";
    src = storePath selfSource;
    patches = [ ];
  };

  spec = {
    binary = "${package}/bin/${package.pname}";
    ldd = "${pkgs.glibc.bin}/bin/ldd";
    native = map nativeComponent native;
    components = [ selfComponent ghcComponent ]
      ++ map haskellComponent haskellClosure
      ++ map nativeComponent native;
  };

  materials = pkgs.runCommand "${package.pname}-${package.version}-release-materials"
    {
      nativeBuildInputs = [ pkgs.python3 ];
      spec = builtins.toJSON spec;
      passAsFile = [ "spec" ];
    } ''
    mkdir $out
    python3 ${./collect.py} "$specPath" $out
    cp ${./LICENSES.md} $out/licenses/README.md
  '';

  base = "${package.pname}-v${package.version}_${pkgs.stdenv.hostPlatform.system}";
in
pkgs.runCommand "${package.pname}-${package.version}-release"
{
  nativeBuildInputs = [ pkgs.gnutar pkgs.gzip ];
  passthru = { inherit materials; };
} ''
  mkdir -p $out work/${base}
  cp ${bundled} work/${base}/${package.pname}
  cp -r ${materials}/licenses work/${base}/licenses
  chmod -R u+w work

  tar_opts="--sort=name --mtime=@1 --owner=0 --group=0 --numeric-owner"
  tar $tar_opts -C work -c ${base} | gzip -n > $out/${base}.tar.gz
  # The source archives are compressed already.
  tar $tar_opts -C ${materials} \
    --transform 's,^sources,${base}-sources,' -cf $out/${base}-sources.tar sources
  cp ${materials}/components.json $out/${base}-components.json
  cd $out
  sha256sum ${base}.tar.gz ${base}-sources.tar ${base}-components.json > ${base}-sha256.txt
''
