{ epkgs, version }:

epkgs.elpaBuild {
    pname = "treesitedit";
    ename = "treesitedit";
    version = version;
    src = [ ./treesitedit.el ];
    packageRequires = [];
    meta = {};
}
