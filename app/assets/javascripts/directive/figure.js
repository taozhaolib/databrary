module.directive('figure', [
	function () {
		var compile = function ($element, $attrs) {
			$element.addClass('figure');

			$element.find('figmedia').first().replaceWith('<div class="media"><div class="inner">' + $element.find('figmedia').first().html() + '</div></div>');

			var $figcaption = $element.find('figcaption').first();

			if ($figcaption.length > 0) {
				$element.attr('caption', $figcaption.html());
				$figcaption.replaceWith('<div class="text" ng-bind-html="caption"></div>');
			} else {
				$element.append('<div class="text" ng-bind-html="caption"></div>')
			}

			var href = ($attrs.ngHref) ? $attrs.ngHref : $attrs.href;

			if (href) {
				var $wrap = $('<a></a>');

				$wrap.attr('ng-href', href);

				if ($attrs.title) {
					$wrap.attr('title', $attr.title);
				}

				$element.wrapInner($wrap);

				$element.attr('href', '');
				$element.attr('ng-href', '');
				$element.attr('title', '');
			}

			return function ($scope, $element, $attrs) {
				$attrs.$observe('caption', function () {
					$scope.caption = $attrs.caption;
				});
			};
		};

		return {
			restrict: 'E',
			priority: 100,
			compile: compile
		};
	}
]);
