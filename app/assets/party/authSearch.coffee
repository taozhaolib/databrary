'use strict'

app.directive 'authSearchForm', [
  'modelService', 'constantService',
  (models, constants) ->
    restrict: 'E'
    templateUrl: 'party/authSearch.html'
    link: ($scope, $element, $attrs) ->
      party = $scope.party || models.Login.user
      form = $scope.authSearchForm

      form.principal = $attrs.principal
      form.apply = form.principal != 'child'

      $scope.$watch ->
          form.principal
        , (principal) ->
          form.validator.client({
              name:{tips: constants.message('auth.search.' + (principal || 'placeholder') + '.help')}
            }, true)
          form.placeholderText = $attrs.placeholderText || constants.message('auth.search.' + (form.principal || 'placeholder'))
          return

      select = (found) -> ->
        form.selectFn(found)
        form.$setPristine()
        ''

      notfound =
        text: constants.message('auth.notfound')
        select: ->
          form.notFoundFn(form.nameVal, form)
          form.$setPristine()
          ''

      form.search = (val) ->
        party.authorizeSearch(form.apply,
            name: val
            institution: form.principal == 'principal' || (if form.principal == 'affiliate' then false else undefined)
          ).then (data) ->
              form.validator.server {}
              
              l = _.map data, (found) ->
                    text: found.name
                    select:select found
                    
              l.push notfound
              l
            , (res) ->
              form.validator.server res
              return

      form.validator.client
          name:
            tips: constants.message('auth.search.name.help'),
        , true

      $scope.$emit('authSearchForm-init', form)

      return
]
