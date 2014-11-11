'use strict';

app.controller('volume/comments', [
  '$scope', 'pageService',
  function ($scope, page) {
    $scope.canPost = page.models.Login.isAuthorized();

    $scope.refreshPanel = function () {
      $scope.comments = $scope.volume.comments;
      $scope.enabled = $scope.canPost || !$.isEmptyObject($scope.comments);
    };

    //

    $scope.pullComments = function () {
      $scope.volume.get(['comments']).then(
        $scope.refreshPanel,
        function (res) {
          page.messages.addError({
            body: page.constants.message('comments.update.error'),
            report: res,
          });
        });
    };

    //

    $scope.commentClass = function (comment) {
      var cls = {};
      if (comment.parents)
        cls['depth-'+Math.min(comment.parents.length, 5)] = true;
      return cls;
    };

    //

    $scope.replyTo = undefined;

    $scope.setReply = function (comment) {
      $scope.replyTo = comment;
    };

    //

    $scope.$on('commentReplyForm-init', function (event, form) {
      form.successFn = $scope.pullComments;
      form.cancelFn = $scope.setReply;
      form.target = $scope.replyTo;
      event.stopPropagation();
    });

  }
]);
