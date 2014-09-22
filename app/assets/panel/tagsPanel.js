'use strict';

module.controller('TagsPanel', [
  '$scope', 'pageService', function ($scope, page) {
    var DEFAULT_MESSAGE = {
      type: 'green',
      countdown: 3000,
    };

    //

    var createMessage = function (message) {
      page.messages.add(angular.extend({}, DEFAULT_MESSAGE, {
          body: message,
      }));
    };

    //

    $scope.tags = [];
    $scope.target = undefined;

    $scope.refreshPanel = function () {
      switch (page.$route.current.$$route.controller) {
        case 'volumeView':
          $scope.prepareTags($scope.volume.tags);
          $scope.target = $scope.volume.top;
          $scope.enabled = $scope.tags.length > 0 || page.models.Login.isLoggedIn();
          break;

        case 'partyView':
          $scope.prepareTags($scope.party.tags);
          $scope.enabled = $scope.tags.length > 0;
          break;
      }
    };

    $scope.prepareTags = function (tags) {
      var temp = [];

      angular.forEach(tags, function (tag) {
        temp.push(tag);
      });

      $scope.tags = temp;

      $scope.sortTags();
    };

    $scope.sortTags = function () {
      $scope.tags = $scope.tags.sort(function (a, b) {
        return (a.weight > b.weight) ? -1 : (a.weight < b.weight) ? 1 : 0;
      });
    };

    $scope.retrieveTags = function () {
      switch (page.$route.current.$$route.controller) {
        case 'volumeView':
	  $scope.volume.get(['tags']).then(
	    $scope.refreshPanel,
	    function (res) {
	      page.messages.addError({
		body: page.constants.message('tags.update.error'),
		report: res,
	      });
	    });

          break;
      }
    };

    //

    $scope.vote = function (tag, vote) {
      $scope.target.setTag(tag.id, vote).then(function (newTag) {
        if (newTag.weight === 0 && !newTag.vote) {
          $scope.tags.remove(tag);
        }
        else {
          $scope.tags.splice($scope.tags.indexOf(tag), 1, newTag);
        }

	var directions = {'-1': 'down', 0: 'null', 1: 'up'};
        createMessage(page.constants.message('tags.vote.'+directions[vote]+'.success', {sce: page.$sce.HTML}, tag.id));
      }, function (res) {
        page.messages.addError({
          body: page.constants.message('tags.vote.error', {sce: page.$sce.HTML}, tag.id),
          report: res,
        });
      });
    };

    $scope.voteNew = function (form) {
      if (form.$invalid || !form.newNameVal) {
        return;
      }

      emptyAuto();

      var tag = form.newNameVal;
      form.newNameVal = '';

      $scope.target.setTag(tag, true).then(function () {
        createMessage(page.constants.message('tags.new.success', {sce: page.$sce.HTML}, tag));
        emptyAuto();

        $scope.retrieveTags();
      }, function (res) {
        page.messages.addError({
          body: page.constants.message('tags.new.error', {sce: page.$sce.HTML}, tag),
          report: res,
        });

        emptyAuto();
      });
    };

    //

    $scope.newNameChange = function (form) {
      if (form.newName.$dirty && form.newName.$valid) {
        updateAuto(form);
      }

      if (form.newName.$pristine || form.newName.$valid) {
        return disableNewNameError();
      }

      return enableNewNameError();
    };

    var keypress = function (event, form) {
      if (event.which == 40) {
        // down
        if (angular.isUndefined($scope.autoSelect) || $scope.autoSelect === $scope.autoList.length - 1) {
          $scope.autoSelect = 0;
        }
        else {
          $scope.autoSelect = $scope.autoSelect + 1;
        }

        return;
      } else if (event.which == 38) {
        // up
        if (angular.isUndefined($scope.autoSelect) || $scope.autoSelect === 0) {
          $scope.autoSelect = $scope.autoList.length - 1;
        }
        else {
          $scope.autoSelect = $scope.autoSelect - 1;
        }

        return;
      } else if (event.which == 13) {
        // enter
        if (angular.isDefined($scope.autoSelect)) {
          $scope.fillAuto(form, $scope.autoList[$scope.autoSelect]);
        }

      } else if (event.which == 27) {
        // escape
        emptyAuto();
      }
    };

    $scope.newNameFocus = function (form) {
      emptyAuto();

      $('#newName').on('keydown', $scope.$lift(function (event) {
	keypress(event, form);
      }));

      $scope.newNameChange(form);
    };

    $scope.newNameBlur = function () {
      $('#newName').off('keypress');

      emptyAutoAfter(250);

      return disableNewNameError();
    };

    $scope.autoList = [];
    $scope.autoSelect = undefined;

    var updating = false;
    var updateAuto = function (form) {
      if (updating)
	return;
      updating = true;
      page.models.Tag.search(form.newNameVal)
	.then(function (data) {
          emptyAuto();

          if (form.newNameVal) {
            $scope.autoList = data;
          }
        }, function (errors, status) {
          page.messages.addError({
	    body: page.constants.message('tags.auto.error', {sce: page.$sce.HTML}),
            errors: errors,
            status: status
          });

          emptyAuto();
        }).finally(function () {
	  updating = false;
	});
    };

    var emptyAuto = function () {
      $scope.autoList = [];
      $scope.autoSelect = undefined;
    };

    var emptyAutoAfter = function (after) {
      page.$timeout(emptyAuto, after);
    };

    $scope.fillAuto = function (form, autoTag) {
      form.newNameVal = autoTag;
      emptyAuto();
      $scope.voteNew(form);
    };

    //

    var enableNewNameError = function () {
      emptyAuto();

      if (!$scope.tagNewFormMessage) {
        var message = {
          enabled: true,
          type: 'red',
          body: page.constants.message('tag.invalid.long', {sce: page.$sce.HTML})
	};

        $scope.tagNewFormMessage = page.messages.add(message);
      }
    };

    var disableNewNameError = function () {
      if ($scope.tagNewFormMessage) {
        $scope.tagNewFormMessage.remove();
	$scope.tagNewFormMessage = undefined;
      }
    };
  }
]);
