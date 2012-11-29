{application, playii,
 [{description, "playii game system"},
  {vsn, "0.1"},
  {modules, [
    playii, playii_app, playii_sup,
    playii_web
  ]},
  {registered, []},
  {mod, {playii_app, []}},
  {env, [
   {web, true},
   {web.max, 102400},
   {web.addr, "0.0.0.0"},
   {web.port, "8080"},
   {player, true},
   {player.max, 102400},
   {scene, true},
   {scene.max, 102400}
  ]},
  {applications, [kernel, stdlib, mnesia]}
 ]}.
