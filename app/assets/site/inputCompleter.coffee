'use strict'

app.directive 'inputCompleter', [
  'constantService',
  (constants) ->
    restrict: 'E'
    templateUrl: 'site/inputCompleter.html'
    scope:
      value: '=ngModel'
      completer: '&'
      placeholder: '@'
      pattern: '@ngPattern'
      submit: '&'
    link: ($scope, $element, $attrs) ->
      # this doesn't happen early enough with normal angular binding:
      $element[0].firstChild.firstChild.setAttribute('name', $attrs.name)
      sent = resend = undefined
      $scope.choices = []

      handle = (r) ->
        if r && typeof r.then == 'function'
          r.then(handle)
        else
          sent = undefined
          if Array.isArray(r)
            if 'input' of r
              $scope.value = r.input
              delete r.input
            if r.length
              $scope.choices = r
            else
              $scope.choices = [
                text: constants.message('search.none')
              ]
          else if r || r == ''
            $scope.value = r
            $scope.choices = []
          $scope.search(resend) if resend

      $scope.search = (input) ->
        if sent
          resend = input != sent && input
        else if input && input.length >= 3
          resend = undefined
          sent = input
          $scope.choices.push
            text: constants.message('search.active')
          handle($scope.completer({$input:input}))
        else
          $scope.choices = []

      $scope.choose = (c) ->
        resend = undefined
        handle(
          if typeof c.select == 'function'
            c.select()
          else
            c.select)

      $scope.enter = (input) ->
        handle($scope.submit({$input:input})) if input
]
