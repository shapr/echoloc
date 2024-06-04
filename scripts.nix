{s}: 
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:echoloc' --allow-eval --warnings";
  testScript = s "test" "cabal run test:echoloc-tests";
  hoogleScript = s "hgl" "hoogle serve";
}
