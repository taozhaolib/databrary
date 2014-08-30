'use strict';

module.factory('browserService', [
  '$rootScope',
  'ArrayHelper',
  'typeService',
  'messageService',
  'constantService',
  'tooltipService',
  '$timeout',
  'displayService',
  function ($rootScope, ArrayHelper, typeService, messages, constants, tooltips, $timeout, display) {
    var browserService = {};

    //

    var DEFAULT_OPTIONS = {
      record: {
        allow: true,
        categories: new ArrayHelper([])
      },

      session: {
        allow: true,
        active: true,
        expand: false,
      }
    };

    var DEFAULT_CATEGORY = {
      allow: true,
      active: false,
      expand: true,
    };

    //

    var volume;

    //

    browserService.options = {};

    browserService.data = {};

    browserService.groups = {};

    //

    browserService.initialize = function (newVolume) {
      browserService.query = '';

      initialize(newVolume);
    };

    var initialize = function (newVolume) {
      volume = newVolume;

      angular.extend(browserService.options, DEFAULT_OPTIONS);

      browserService.options.record.categories = new ArrayHelper(
        volume.categories.map(function (category) {
          return angular.extend({
            id: category,
            name: constants.data.category[category].name,
          }, DEFAULT_CATEGORY);
        }));

      rebuildData();
    };

    //

    var focus, focusInvert, focusPosition;

    var rebuildData = function (focusGroup) {
      if (focusGroup) {
        focusInvert = (focus == focusGroup && getLevelByGroup(focusGroup.id) == focusPosition) ? !focusInvert : undefined;
        focusPosition = getLevelByGroup(focusGroup.id);
      }

      focus = focusGroup;

      var groups = getActiveGroups(),
        data = {
          items: [],
          level: -1,
          group: 'browser',
          limit: 20
        };

      browserService.groups = {};

      angular.forEach(groups, function (group) {
        browserService.groups[group] = [];
      });

      if (groups[0] == 'session')
        callbackSessions(data, volume);
      else
        callbackRecords(data, volume, groups);

      angular.extend(browserService.data, data);

      return data;
    };

    var updateData = function (data) {
      if (!data.object) {
        return undefined;
      }

      var groups = getActiveGroups();

      if (!groups[data.level]) {
        return undefined;
      }

      if (data.group == 'session')
        callbackSessionChildren(data);
      else
        callbackRecordChildren(data, data.volume, groups);

      return data;
    };

    var isGroupAllowed = function (group) {
      return browserService.options[group] && browserService.options[group].allow;
    };

    var isGroupActive = function (group) {
      return isGroupAllowed(group) && browserService.options[group].active;
    };

    var getActiveGroups = function () {
      var groups = [];

      groups.push.apply(groups, getActiveRecordGroups());

      if (isGroupActive('session')) {
        groups.push('session');
      }

      return groups;
    };

    var getAllowedGroups = function () {
      var groups = [];

      groups.push.apply(groups, getAllowedRecordGroups());

      if (isGroupAllowed('session')) {
        groups.push('session');
      }

      return groups;
    };

    var getActiveRecordGroups = function () {
      var groups = [];

      angular.forEach(browserService.options.record.categories, function (category) {
        if (category.allow && category.active) {
          groups.push(category.id);
        }
      });

      return groups;
    };

    var getAllowedRecordGroups = function () {
      var groups = [];

      angular.forEach(browserService.options.record.categories, function (category) {
        if (category.allow) {
          groups.push(category.id);
        }
      });

      return groups;
    };

    browserService.isLastGroup = function (group) {
      var groups = getAllowedGroups();

      return groups.indexOf(group) == groups.length - 1;
    };

    browserService.showList = function (data) {
      return !!getActiveGroups()[data.level + 1];
    };

    //

    var callbackRecords = function (data, volume, groups) {
      var category = groups[data.level + 1];
      var tempData = {};
      var sessions = data.sessions || volume.sessions;

      angular.forEach(sessions, function (session, sid) {
        var any = false;
        angular.forEach(session.records, function (record) {
          if (volume.records[record.id].category == category) {
            if (!(record.id in tempData))
              tempData[record.id] = {};
            tempData[record.id][sid] = session;
            any = true;
          }
        });

        if (!any) {
          if (!('null' in tempData))
            tempData['null'] = {};
          tempData['null'][sid] = session;
        }
      });

      if (!$.isEmptyObject(tempData)) {
        angular.forEach(tempData, function (newSessions, recordID) {
          var newData;

          if (recordID != 'null') {
            newData = callbackItem(data, volume, newSessions, volume.records[recordID], category);
          }
          else {
            newData = callbackItem(data, volume, newSessions, {
              category: category,
              id: 0,
              measures: {}
            }, category);
          }

          callbackRecordChildren(newData, volume, groups);
        });

        data.items.reverse();
      }

      return data;
    };

    var callbackRecordChildren = function (data, volume, groups) {
      if (!browserService.getItemExpand(data)) {
        return data;
      }

      if (groups[data.level + 1] == 'session') {
        callbackSessions(data, volume);
      } else {
        callbackRecords(data, volume, groups);
      }

      return data;
    };

    var callbackSessions = function (data, volume) {
      var sessions = data.sessions || volume.sessions;

      angular.forEach(sessions, function (session) {
        var newData = callbackItem(data, volume, undefined, session, 'session');

        callbackSessionChildren(newData);
      });

      return data;
    };

    var callbackSessionChildren = function (data) {
      if (!browserService.getItemExpand(data)) {
        return data;
      }

      browserService.loading = false;
    };

    var callbackItem = function (data, volume, sessions, object, group) {
      var option = getOptionByGroup(group);

      var id = 'data-' + group + '-' + object.id;

      var newData = {
        parent: data,
        volume: volume,
        sessions: sessions,
        level: data.level + 1,

        id: id,
        object: object,
        permission: object.permission || volume.permission,
        group: group,
        items: [],

        select: false,
        expand: (focus && focus.id == group) ? ((angular.isDefined(focusInvert)) ? focusInvert : false) : option.expand,
        limit: 10
      };

      if (group == 'session') {
        var newSegment;
        var records = volume.sessions[newData.object.id].records;
        var cur, obj;
        var union = function (seg, c) {
          if (c.id === obj.id) {
            /* if record coverage is disjoint we pretend it's continuous: */
            seg = typeService.segmentUnion(seg, c.segment);
          }
          return seg;
        };
        for (cur = newData.parent; (obj = cur.object); cur = cur.parent) {
          if (obj.id !== 0)
            newSegment = typeService.segmentIntersect(newSegment,
              records.reduce(union, null));
        }
        newData.segment = newSegment;
        if (typeService.segmentEmpty(newSegment)) {
          return newData; //in order to not push empty segmented things (contradictory constraints) onto list
        }
      }

      browserService.groups[group].push(newData);

      if (group == 'session' && object.top) {
        data.items.unshift(newData);
      }
      else {
        data.items.push(newData);
      }

      return newData;
    };

    //

    var recordGroupToggle;

    browserService.setRecordGroupToggle = function (group) {
      if (group == 'add') {
        var c = 0, maybe;

        angular.forEach(browserService.options.record.categories, function (recordGroup) {
          if (!recordGroup.active) {
            maybe = recordGroup;
            c++;
          }
        });

        if (c == 1) {
          browserService.addRecordGroup(maybe);
          return true;
        }
      }

      recordGroupToggle = angular.isUndefined(recordGroupToggle) ? group : undefined;
    };

    browserService.clearRecordGroupToggle = function () {
      if (angular.isDefined(recordGroupToggle)) {
        recordGroupToggle = undefined;
      }
    };

    browserService.isRecordGroupToggle = function (group) {
      return recordGroupToggle == group;
    };

    //

    browserService.canAddRecordGroup = function () {
      var canAdd = false;

      angular.forEach(browserService.options.record.categories, function (recordGroup) {
        if (!canAdd && !recordGroup.active) {
          canAdd = true;
        }
      });

      return canAdd;
    };

    browserService.addRecordGroup = function (group) {
      browserService.setRecordGroupToggle(undefined);

      var i = browserService.options.record.categories.index(group);

      group.active = true;

      browserService.options.record.categories.push(browserService.options.record.categories.splice(i, 1)[0]);

      rebuildData(group);
    };

    browserService.canRemoveRecordGroup = function () {
      return true;
    };

    browserService.removeRecordGroup = function (group) {
      group.active = false;

      var group_i = browserService.options.record.categories.index(group);

      browserService.options.record.categories.splice(group_i, 1);
      browserService.options.record.categories.push(group);

      rebuildData();
    };

    browserService.switchRecordGroup = function (group, maybe) {
      browserService.setRecordGroupToggle(undefined);

      var group_i = browserService.options.record.categories.index(group),
        maybe_i = browserService.options.record.categories.index(maybe);

      if (group.active != maybe.active) {
        group.active = !group.active;
        maybe.active = !maybe.active;
      }

      browserService.options.record.categories[group_i] = browserService.options.record.categories.splice(maybe_i, 1, browserService.options.record.categories[group_i])[0];

      rebuildData(maybe);
    };

    //

    browserService.setItemExpand = function (data, expand) {
      if (!data.expand && expand !== false) {
        data.expand = true;

        if (!Array.isArray(data.items) || data.items.length === 0) {
          updateData(data);
        }
      } else if (data.expand && expand !== true) {
        data.expand = false;

        if (data == browserService.player) {
          browserService.setItemPlayer(undefined);
        }
      }

      return data;
    };

    browserService.getItemExpand = function (data) {
      return data.expand;
    };

    browserService.canExpand = function (data) {
      return data.level >= 0 && getActiveGroups()[data.level + 1];
    };

    //

    var getOptionByGroup = function (group) {
      if (group == 'session')
        return browserService.options.session;
      else
        return browserService.options.record.categories.find({id: group});
    };

    var getLevelByGroup = function (group) {
      return getActiveGroups().indexOf(group);
    };

    //

    browserService.player = undefined;

    browserService.setItemPlayer = function (data) {
      var newPlayer, newPlayed;

      if (angular.isUndefined(data)) {
        newPlayed = undefined;
        newPlayer = undefined;
      } else {
        newPlayed = data.items[0] || undefined;
        newPlayer = data;
      }

      if (angular.isUndefined(browserService.player)) {
        browserService.player = newPlayer;

        browserService.player.player = true;

        browserService.player.played = newPlayed;
      } else if (browserService.player != newPlayer) {
        browserService.player.player = false;

        browserService.player = newPlayer;

        if (angular.isDefined(browserService.player)) {
          browserService.player.player = true;
          browserService.player.played = newPlayed;
        }
      } else if (browserService.player.played != newPlayed) {
        browserService.player.played = newPlayed;
      } else {
        browserService.player.player = false;

        browserService.player = undefined;
      }

      if (data && data.parent && data.parent.id)
        display.scrollTo($('#' + data.parent.id).find('.browser-controller'));

      return browserService.player;
    };

    //

    return browserService;
  }
]);
