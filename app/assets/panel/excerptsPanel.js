'use strict';

module.controller('ExcerptsPanel', [
  '$scope',
  'pageService',
  function ($scope, page) {
    $scope.bootPanel = function () {
      if (Array.isArray($scope.volume.excerpts) && $scope.volume.excerpts.length > 0) {
        $scope.current = $scope.volume.excerpts[0] || undefined;
      }
    };

    $scope.refreshPanel = function () {
      $scope.enabled = Array.isArray($scope.volume.excerpts) && $scope.volume.excerpts.length > 0;
    };

    //

    $scope.setCurrent = function (asset) {
      $scope.current = asset;
    };

    $scope.getMimeGroup = function (asset) {
      var mimetype = page.types.assetFormat(asset).mimetype,
        type = mimetype.split('/')[0];

      return type == 'text' ? mimetype[1] : type;
    };

    $scope.hasThumbnail = function (asset) {
      return $scope.getMimeGroup(asset) == 'image' || $scope.getMimeGroup(asset) == 'video';
    };

    $scope.showThumbnail = function (asset) {
      return $scope.getMimeGroup(asset) == 'image' || asset.asset.duration;
    };

    $scope.listClass = function (excerpt) {
      var cls = [];

      if ($scope.getMimeGroup(excerpt) == 'video') {
        cls.push('video');
      }

      if (excerpt === $scope.current) {
        cls.push('panel-excerpts-list-current');
      }

      return cls;
    };

    $scope.jump = function (asset) {
      var found;

      for (var i = 0, l = page.browser.groups.session.length; i < l; i++) {
        if (page.browser.groups.session[i].object.id == asset.container.id) {
          found = page.browser.groups.session[i];
          break;
        }
      }

      if (!found) {
        return expandTo(asset);
      }

      var $item = $('#' + found.id);

      if ($item.length === 0) {
        return addTo(found, asset);
      }

      page.display.scrollTo($item);
      page.browser.setItemExpand(found, true);
    };

    var expandTo = function (asset) {
      var dirty;
      var records = $scope.volume.sessions[asset.container.id].records;
      var volumeRecords = $scope.volume.records;

      angular.forEach(page.browser.groups, function (objects, group) {
        if (!$.isNumeric(group)) {
          return;
        }

        /* XXX */
        var recordIDs = records.filter(function (rec) {
          return page.types.segmentOverlaps(asset.segment, rec.segment) && volumeRecords[rec.id].category === group;
        }).map(function (obj) {
          return obj.id;
        });
        if (!recordIDs.length)
          recordIDs = [0];

        angular.forEach(objects, function (data) {
          if (recordIDs.indexOf(data.object.id) > -1) {
            page.browser.setItemExpand(data, true);
            dirty = true;
          }
        });
      });

      if (dirty) {
        page.$timeout(function () {
          $scope.jump(asset);
        }, 1);
      }
    };

    var addTo = function (session, asset) {
      var data = session.parent, index;

      for (var i = 0, l = data.items.length; i < l; i++) {
        if (data.items[i] == session) {
          index = i;
          break;
        }
      }

      if (index) {
        data.items.splice(9, 0, data.items.splice(index, 1)[0]);

        page.$timeout(function () {
          $scope.jump(asset);
        }, 1);
      }
    };
  }
]);
