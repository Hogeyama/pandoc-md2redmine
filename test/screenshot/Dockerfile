FROM redmica/redmica:3.1.7
# RedMica 3.1.7 itself does not render Mermaid; v0.6.0 of this plugin bundles
# mermaid.js and renders the {{mermaid}} macro.
RUN git clone --depth 1 -b v0.6.0 https://github.com/redmica/redmica_ui_extension.git plugins/redmica_ui_extension
