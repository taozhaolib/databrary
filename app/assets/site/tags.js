'use strict';

app.controller('site/tags', [
  '$scope', 'pageService', function ($scope, page) {

    function createMessage(message) {
      page.messages.add({
        type: 'green',
        countdown: 3000,
        body: message,
      });
    }

    $scope.tags = [];

    function sortTags() {
      $scope.tags.sort(function (a, b) {
        return b.weight - a.weight;
      });
    }

    function prepareTags(tags) {
      var temp = [];

      angular.forEach(tags, function (tag) {
        temp.push(tag);
      });

      $scope.tags = temp;
      sortTags();
    }

    $scope.refreshPanel = function () {
      switch (page.$route.current.controller) {
        case 'volume/view':
          prepareTags($scope.volume.tags);
          if (page.models.Login.isAuthorized()) {
            $scope.target = $scope.volume.top;
            $scope.enabled = true;
          } else
            $scope.enabled = $scope.tags.length;
          break;

        case 'party/view':
          prepareTags($scope.party.tags);
          $scope.enabled = $scope.tags.length;
          break;
      }
    };

    $scope.vote = function (name, vote) {
      return $scope.target.setTag(name, vote).then(function (tag) {
        var i = $scope.tags.findIndex(function (t) { return t.id === tag.id; });
        if (i === -1)
          i = $scope.tags.length;
        if (tag.weight)
          $scope.tags[i] = tag;
        else
          $scope.tags.splice(i, 1);

        createMessage(page.constants.message('tags.vote.'+(vote ? 'up' : 'null')+'.success', {sce: page.$sce.HTML}, tag.id));
        page.tooltips.clear(); // hack for broken tooltips
      }, function (res) {
        page.messages.addError({
          body: page.constants.message('tags.vote.error', {sce: page.$sce.HTML}, name),
          report: res,
        });
      });
    };
  }
]);
