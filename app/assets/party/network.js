'use strict';

app.directive('partyNetwork', [
  'pageService',
  function (page) {
    return {
    restrict: 'E',
    templateUrl: 'party/network.html',
    scope: false,
    link: function ($scope) {
      var actionMessages = {};

      $scope.$on('$destroy', function () {
        _.each(actionMessages, function (bundle) {
          bundle.message.remove();
        });
      });

      $scope.isAdmin = $scope.party.checkPermission(page.permission.ADMIN);

      if ($scope.isAdmin)
      _.each($scope.party.children, function (party) {
        if (!party.member && !party.site) {
          if (!actionMessages[party.id]) {
            actionMessages[party.id] = {
              party: party,
              message: page.messages.add({
                type: 'yellow',
                persist: true,
                body: page.$sce.trustAsHtml('<span>' + page.constants.message('auth.pending.notice', {sce:page.$sce.HTML}, party.party.name) + ' <a href="' + $scope.party.editRoute('grant') + '#auth-' + party.party.id + '">Manage</a>.</span>')
              })
            };
          }
          else {
            actionMessages[party.id].party = party;
          }
        }
      });

      //

      var user = page.models.Login.user.id;
      function isUser(a) {
        return a.party.id === user;
      }

      $scope.isParent =
        $scope.party.parents.some(isUser);
      $scope.isRelation = $scope.isParent ||
        /* you always exist on your own page */
        $scope.party.id <= 0 || $scope.party.id === user ||
        $scope.party.children.some(isUser);

      $scope.grant = function () {
        page.$location.url(page.models.Login.user.editRoute('grant'));
        var remove = page.$rootScope.$on('partyEditGrantForm-init', function (event, form) {
          form.preSelect($scope.party);
          remove();
        });
      };

      $scope.apply = function () {
        page.$location.url(page.models.Login.user.editRoute('apply'));
        var remove = page.$rootScope.$on('partyEditApplyForm-init', function (event, form) {
          form.preSelect($scope.party);
          remove();
        });
      };
    }
    };
  }
]);
