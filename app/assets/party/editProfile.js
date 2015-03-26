'use strict';

app.directive('partyEditProfileForm', [
  'pageService', function (page) {
    var link = function ($scope) {
      var party = $scope.party;
      var form = $scope.partyEditProfileForm;

      var fields = ['prename', 'sortname', 'affiliation', 'orcid', 'url'];

      function init() {
        form.data = {};
        fields.forEach(function (f) {
          form.data[f] = party[f];
        });
        form.avatarUrl = party.avatarRoute();
      }
      init();

      form.save = function () {
        page.messages.clear(form);
        var fd, upload;
        if (angular.isObject(form.data.avatar)) {
          fd = new FormData();

          fd.append('avatar', form.data.avatar[0]);

          for (var prop in form.data)
            if (form.data.hasOwnProperty(prop) && form.data[prop] !== undefined)
              fd.append(prop, form.data[prop]);

          upload = page.messages.add({
            type: 'yellow',
            body: page.constants.message('party.edit.avatar.upload', page.constants.message('avatar')),
            owner: form
          });
        } else
          fd = form.data;
        
        party.save(fd)
          .then(function () {
            form.validator.server({});

            page.messages.add({
              type: 'green',
              body: page.constants.message('party.edit.profile.success'),
              owner: form
            });

            if (upload)
              upload.remove();

            init();
            form.$setPristine();

            if (upload)
              form.avatarUrl = party.avatarRoute(undefined, Date.now());
          }, function (res) {
            form.validator.server(res);

            if (upload)
              upload.remove();
          });
      };

      var validate = {};
      fields.forEach(function (f) {
        validate[f] = {
          tips: page.constants.message('party.edit.' + f + '.help')
        };
      });
      form.validator.client(validate, true);
    };

    return {
      restrict: 'E',
      templateUrl: 'party/editProfile.html',
      link: link
    };
  }
]);
