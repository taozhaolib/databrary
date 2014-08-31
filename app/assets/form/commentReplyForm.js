'use strict';

module.directive('commentReplyForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var form = $scope.commentReplyForm;

      form.data = {
        text: ''
      };
      form.target = undefined;

      //

      form.saveFn = undefined;
      form.successFn = undefined;
      form.errorFn = undefined;

      form.save = function () {
        if (angular.isFunction(form.saveFn)) {
          form.saveFn(form);
        }

	(form.target || $scope.volume.top).postComment(form.data)
          .then(function () {
            form.validator.server({});

            form.messages.add({
              body: page.constants.message('comments.add.success'),
              type: 'green',
              countdown: 3000
            });

            if (angular.isFunction(form.successFn)) {
              form.successFn(form, arguments);
            }

            form.cancel();
          }, function (res) {
            form.validator.server(res);

            if (angular.isFunction(form.errorFn)) {
              form.errorFn(form, arguments);
            }
          });
      };

      //

      form.cancelFn = undefined;

      form.cancel = function () {
        if (angular.isFunction(form.cancelFn)) {
          form.cancelFn(form);
        }

        form.data.text = '';
        form.target = undefined;
      };

      //

      form.ready = function () {
        return form.$dirty && form.$valid && form.data.text;
      };

      //

      form.validator.client({
        text: {
          tips: page.constants.message('comments.text.help'),
          errors: page.constants.message('comments.text.error'),
        }
      }, true);

      //

      page.events.talk('commentReplyForm-init', form, $scope);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'commentReplyForm.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
