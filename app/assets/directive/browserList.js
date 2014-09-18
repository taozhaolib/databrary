'use strict';

module.directive('browserList', [
  'pageService', function (page) {
    var link = function ($scope) {
      $scope.data = $scope.data || page.browser.data;

      $scope.getInclude = function () {
        if (!$scope.data.items[0]) {
          return '';
        }

        switch ($scope.data.items[0].group) {
          case 'volume':
            return 'browserVolume.html';

          case 'session':
            return 'browserSession.html';

          case 'asset':
            return 'browserAsset.html';

          default:
            return 'browserRecord.html';
        }
      };

      $scope.itemClasses = function (data) {
        var classes = [];

        if (!data.expand) {
          classes.push('deepest');
        }

        return classes;
      };

      //

      $scope.setItemPlayer = function (data) {
        if (!data || page.models.Login.checkAccess(page.permission.READ, data)) {
          page.browser.setItemPlayer(data);
        }
        else if (data) {
          page.messages.add({
            type: 'yellow',
            countdown: 2000,
            body: page.constants.message('browser.noaccess')
          });
        }
      };

      //

      $scope.formatSessionCategory = function (data, categoryID, records) {
        var category = page.constants.category[categoryID];

        if (!category) {
          return 'Uncategorized';
        }

        if (!records[1]) {
          return $scope.capitalize(category.name);
        }
        else {
          switch (category.name) {
            default:
              return $scope.capitalize(category.name) + 's';
          }
        }
      };

      $scope.capitalize = function (input) {
        return input.charAt(0).toUpperCase() + input.slice(1);
      };

      $scope.getMeasures = function (data) {
        var ident = page.constants.category[data.object.category].ident.concat(page.metric.description.id);

        var measures = [];
        angular.forEach(data.object.measures, function (datum, metric) {
          if (ident.indexOf(parseInt(metric)) === -1)
            measures.push({
              metric: page.constants.metric[metric],
              datum: datum
            });
        });

        return measures;
      };

      $scope.getSessionRecords = function (data) {
        if ('sessionRecords' in data)
          return data.sessionRecords;

        var records = {};
        var categories = [];
        angular.forEach(data.object.records, function (rec) {
          if (page.types.segmentOverlaps(rec.segment, data.segment)) {
            var record = $scope.volume.records[rec.id];
            if (!(record.category in records)) {
              records[record.category] = [];
              categories.push(record.category);
            }
            records[record.category].push(record);
          }
        });

        return (data.sessionRecords = categories.sort(function (a, b) {
          return a - b;
        }).map(function (cat) {
          return { id: cat, records: records[cat] };
        }));
      };

      $scope.nameRecord = function (data) {
        var category = page.constants.category[data.object.category];
	var name;

        if (data.object.id === 0) {
          name = category.not;
        } else {
          name = $scope.capitalize(category.name);
        }

        var identifier = data.object.displayName;

        if (identifier) {
          name += ': ' + identifier;
        }

        return name;
      };
    };

    return {
      restrict: 'E',
      scope: false,
      templateUrl: 'browserList.html',
      replace: true,
      priority: 100,
      link: link
    };
  }
]);
