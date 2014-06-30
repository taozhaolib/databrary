module.directive('message', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			page.messages.add({
				id: $attrs.id,
				type: $attrs.type,
				target: $attrs.target,
				closeable: $attrs.closeable,
				countdown: $attrs.countdown,
				enabled: $attrs.enabled,
				body: $attrs.body || $element.html()
			});

			$element.remove();
		};

		return {
			restrict: 'EA',
			link: link
		}
	}
]);
