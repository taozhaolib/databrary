module.factory('guiService', [
	'$rootScope',
	'$timeout',
	function ($rootScope, $timeout) {
		var gui = {};

		//

		var $scroll = $('html,body');

		gui.scrollTo = function (id) {
			$timeout(function () {
				$scroll.scrollTop($('#' + id).offset().top - 72);
			}, 1);
		};

		//

		return gui;
	}
]);
