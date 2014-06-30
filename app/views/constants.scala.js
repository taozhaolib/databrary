@(constants : play.api.libs.json.JsValue)
module.constant('constantData',@format.raw(constants.toString));
