let Package = (./schema.dhall).Package

in  [ Package::{
      , pname = "org-multi-wiki"
      , version = "0.4"
      , emacsVersion = "26.1"
      , files = [ "org-multi-wiki.el" ]
      , dependencies = [ "dash", "s", "org-ql" ]
      , recipe =
          ''
          (org-multi-wiki :fetcher github :repo "akirak/org-multi-wiki"
             :files ("org-multi-wiki.el"))
          ''
      }
    , Package::{
      , pname = "helm-org-multi-wiki"
      , version = "0.3.2"
      , emacsVersion = "26.1"
      , files = [ "helm-org-multi-wiki.el" ]
      , dependencies =
        [ "helm", "org-multi-wiki", "org-ql", "dash", "helm-org" ]
      , localDependencies = [ "org-multi-wiki" ]
      , buttercupTests = [] : List Text
      , recipe =
          ''
          (helm-org-multi-wiki :fetcher github :repo "akirak/org-multi-wiki"
             :files ("helm-org-multi-wiki.el"))
          ''
      }
    ]