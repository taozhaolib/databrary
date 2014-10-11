@(constants : play.api.libs.json.JsValue, routes : JavaScript)
var constants=@format.raw(constants.toString);app.constant('constantData',constants);
@routes;app.constant('routeData',Object.freeze(routes).controllers);
