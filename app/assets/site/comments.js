'use strict';

module.controller('site/comments', [
  '$scope', 'pageService', '$sanitize',
  function ($scope, page, $sanitize) {
    var form;
    page.events.listen($scope, 'commentReplyForm-init', function (event, commentForm) {
      form = commentForm;
    });

    //

    $scope.refreshPanel = function () {
      switch (page.$route.current.controller) {
        case 'volumeView':
          $scope.comments = $scope.volume.comments;

          $scope.enabled = page.models.Login.isLoggedIn() || !$.isEmptyObject($scope.comments);
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
	  $scope.volume.get(['comments']).then(
	    $scope.refreshPanel,
	    function (res) {
	      form.messages.addError({
		body: page.constants.message('comments.update.error'),
		report: res,
	      });
	    });

          break;
      }
    };

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
      var isParty = page.$route.current.controller === 'partyView' && !$scope.volume;
      var isTop = comment.container.top;

      var meta = '<time datetime="' + page.$filter('date')(comment.time, 'yyyy-MM-dd HH:mm:ss Z') + '" pubdate>' + page.$filter('date')(comment.time, 'MMMM d, yyyy') + '</time>';

      if (isTop && !isParty) {
        return meta;
      }

      meta += ' <span class="sep">|</span>';

      var volume = comment.volume;

      if (isParty) {
        meta += ' <a href="' + volume.route + '">' + $sanitize(page.$filter('truncate')(volume.name, 20)) + '</a>';
      }

      if (isParty && !isTop) {
        meta += ' <span class="sep">/</span>';
      }

      if (!isTop) {
        meta += ' <a href="' + volume.route + '"><img class="line" src="' + volume.thumbRoute() + '"> ' + $sanitize(comment.container.name || '') + '</a>';
      }

      return meta;
    };

    //

    var commentReplyForm;
    var replyTo;

    $scope.getReply = function (comment) {
      return page.models.Login.isLoggedIn() &&
        page.$route.current.controller != 'partyView' &&
        replyTo == comment;
    };

    $scope.setReply = function (comment) {
      replyTo = comment;
    };

    //

    page.events.listen($scope, 'commentReplyForm-init', function (event, form) {
      commentReplyForm = form;
      form.successFn = $scope.pullComments;
      form.cancelFn = $scope.setReply;
      form.target = replyTo;
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
