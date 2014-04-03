define(['config/module'], function (module) {
	'use strict';

	module.directive('commentReplyForm', ['Comment', 'AuthService', 'EventService', 'TypeService', 'Page', function (Comment, authService, eventService, type, page) {
		var link = function ($scope) {
			var form = $scope.commentReplyForm;

			form.getParty = function () {
				return authService.user;
			};

			form.data = {
				text: '',
				container: $scope.volume && $scope.volume.top.id,
				segment: ',',
				parent: undefined
			};

			//

			form.saveFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;

			form.save = function () {
				if (angular.isFunction(form.saveFn))
					form.saveFn(form);

				form.comment = new Comment();

				form.comment.$save(form.data, function () {
					page.messages.add({
						body: page.constants.message('comments.add.success'),
						type: 'green'
					});

					if (angular.isFunction(form.successFn))
						form.successFn(form, arguments);

					form.cancel();
				}, function (res) {
					page.messages.addError({
						body: page.constants.message('comments.add.error'),
						errors: res[0],
						status: res[1]
					});

					if (angular.isFunction(form.errorFn))
						form.errorFn(form, arguments);
				});
			};

			//

			form.cancelFn = undefined;

			form.cancel = function () {
				if (angular.isFunction(form.cancelFn))
					form.cancelFn(form);

				form.data.text = '';
				form.target(undefined, undefined);
			};

			//

			form.target = function (comment) {
				if (comment) {
					form.data.container = comment.container.id;
					form.data.segment = type.segmentString(comment);
					form.data.parent = comment.id;
					return;
				}

				form.data.container = $scope.volume && $scope.volume.top.id;
				form.data.segment = ',';
				form.data.parent = undefined;
			};

			form.ready = function () {
				return form.$dirty && form.$valid && form.data.text;
			};

			//

			eventService.talk('commentReplyForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'commentReplyForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}]);
});
