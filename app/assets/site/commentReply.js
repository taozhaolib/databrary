'use strict';

app.directive('commentReplyForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var form = $scope.commentReplyForm;

      form.data = {
        text: ''
      };
      form.target = undefined;

      //

      form.successFn = undefined;

      form.save = function () {
        (form.target || $scope.volume.top).postComment(form.data)
          .then(function () {
            form.validator.server({});

            form.messages.add({
              body: page.constants.message('comments.add.success'),
              type: 'green',
              countdown: 3000
            });

            if (angular.isFunction(form.successFn)) {
              form.successFn();
            }

            form.cancel();
          }, function (res) {
            form.validator.server(res);
          });
      };

      //

      form.cancelFn = undefined;

      form.cancel = function () {
        if (angular.isFunction(form.cancelFn)) {
          form.cancelFn();
        }

        form.data.text = '';
        form.target = undefined;
      };

      //

      form.validator.client({
        text: {
          tips: page.constants.message('comments.text.help'),
          errors: page.constants.message('comments.text.error'),
        }
      }, true);

      //

      $scope.$emit('commentReplyForm-init', form);
    };

    //

    return {
      restrict: 'E',
      templateUrl: 'site/commentReply.html',
      scope: false,
      replace: true,
      link: link
    };
  }
]);
