define(['app/config/module'], function (module) {
	'use strict';

	module.directive('figure', [function () {
		var compile = function ($element, $attrs) {
			$element.addClass('figure');

			var href = ($attrs.ngHref) ? $attrs.ngHref : $attrs.href;

			if (href) {
				var $wrap = $('<a></a>');

				$wrap.attr('ng-href', href);

				if ($attrs.title)
					$wrap.attr('title', $attr.title);

				$element.wrapInner($wrap);

				$element.attr('href', '');
				$element.attr('ng-href', '');
				$element.attr('title', '');
			}

			$element.find('figmedia').first().replaceWith('<div class="media"><div class="inner">'+$element.find('figmedia').first().html()+'</div></div>');
			$element.find('figcaption').first().replaceWith('<div class="text">'+$element.find('figcaption').first().html()+'</div>');
		};

		return {
			restrict: 'E',
			priority: 100,
			compile: compile
		};
	}]);
});
