@(constants : play.api.libs.json.JsValue, routes : JavaScript)
module.constant('constantData',@format.raw(constants.toString));
@routes;module.constant('routeData',Object.freeze(routes).controllers);
