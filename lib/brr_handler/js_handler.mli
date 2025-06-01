val send :
  (module Services.JsonClientHandler
     with type request = 'request
      and type response = 'response
      and type placeholders = 'placeholders) ->
  'placeholders ->
  'request ->
  ('response, Jv.Error.t) Fut.result
