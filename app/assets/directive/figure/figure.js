module.directive('figure', [
	function () {
		var compile = function ($element, $attrs) {
			$element.find('figmedia').first().replaceWith('<div class="figmedia"><div class="figmedia-inner">' + $element.find('figmedia').first().html() + '</div></div>');

			if ($attrs.href) {
				var $wrap = $('<a></a>');

				$wrap.attr('ng-href', $attrs.href);
				$element.attr('href', '');

				if ($attrs.title) {
					$wrap.attr('title', $attr.title);
					$element.attr('title', '');
				}

				$element.wrapInner($wrap);
			}
		};

		return {
			restrict: 'E',
			priority: 100,
			compile: compile
		};
	}
]);
