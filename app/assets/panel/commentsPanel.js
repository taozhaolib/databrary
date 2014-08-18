'use strict';

module.controller('CommentsPanel', [
  '$scope', 'pageService', function ($scope, page) {
    var form;
    page.events.listen($scope, 'commentReplyForm-init', function (event, commentForm) {
      form = commentForm;
    });

    //

    $scope.refreshPanel = function () {
      switch (page.$route.current.controller) {
        case 'volumeView':
          $scope.comments = $scope.volume.comments;

          $scope.enabled = page.auth.isLoggedIn() || !$.isEmptyObject($scope.comments);
          break;

        case 'partyView':
          $scope.comments = $scope.party.comments;

          $scope.enabled = !$.isEmptyObject($scope.comments);
          break;
      }
    };

    //

    $scope.pullComments = function () {
      switch (page.$route.current.controller) {
        case 'volumeView':
          page.models.volume.$cache.removeAll();

          page.models.volume.get({
            id: $scope.volume.id,
            comments: ''
          }, function (data) {
            $scope.volume.comments = data.comments;
            $scope.refreshPanel();
          }, function (res) {
            form.messages.addError({
              body: page.constants.message('comments.update.error'),
              report: res,
            });
          });

          break;
      }
    };

    //

    $scope.authService = page.auth;
    $scope.routeController = page.$route.current.controller;

    //

    $scope.commentParty = function (comment) {
      switch (page.$route.current.controller) {
        case 'partyView':
          return $scope.party;

        default:
          return comment.who;
      }
    };

    $scope.commentMeta = function (comment) {
      var isParty = page.$route.current.controller == 'partyView' && !$scope.volume;
      var isTop = comment.container.top;

      var meta = '<time datetime="' + page.$filter('date')(comment.time, 'yyyy-MM-dd HH:mm:ss Z') + '" pubdate>' + page.$filter('date')(comment.time, 'MMMM d, yyyy') + '</time>';

      if (isTop && !isParty) {
        return meta;
      }

      meta += ' <span class="sep">|</span>';

      var volumeID = isParty ?
        (comment.volume ? comment.volume.id : 0) :
        ($scope.volume ? $scope.volume.id : 0);

      if (isParty) {
        meta += ' <a href="' + page.router.volume(volumeID) + '">' + page.$filter('truncate')(comment.volume.name || $scope.volume.name, 20) + '</a>';
      }

      if (isParty && !isTop) {
        meta += ' <span class="sep">/</span>';
      }

      if (!isTop) {
        meta += ' <a href="' + page.router.volume(volumeID) + '"><img class="line" src="' + page.router.volumeThumb(volumeID) + '"> ' + (comment.container.name || '') + '</a>';
      }

      return meta;
    };

    //

    var commentReplyForm;
    var replyTo;

    $scope.getReply = function (comment) {
      return page.auth.isLoggedIn() &&
        page.$route.current.controller != 'partyView' &&
        replyTo == comment;
    };

    $scope.setReply = function (comment) {
      replyTo = comment;
    };

    //

    var successFn = function () {
      $scope.pullComments();
    };

    var cancelFn = function () {
      $scope.setReply(undefined);
    };

    page.events.listen($scope, 'commentReplyForm-init', function (event, form) {
      commentReplyForm = form;
      form.successFn = successFn;
      form.cancelFn = cancelFn;
      form.target(replyTo);
      event.stopPropagation();
    });

    //

    var parents = [0];

    $scope.getCommentClasses = function (comment) {
      var classes = [];

      if (page.$route.current.controller != 'partyView') {
        if (!comment.parent) {
          comment.parent = 0;
        }

        var index = parents.indexOf(comment.parent);

        if (index > -1) {
          parents = parents.slice(0, index + 1);
        }
        else {
          parents.push(comment.parent);
        }

        if (parents.length >= 5) {
          comment.stop = true;
        }

        classes.push('depth-' + (parents.length - 1));
      } else {
        comment.stop = true;
      }

      return classes;
    };
  }
]);
