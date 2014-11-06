'use strict'

app.controller('volume/slot', [
  '$scope', '$rootScope', '$location', '$q', '$filter', 'constantService', 'displayService', 'messageService', 'Segment', 'Store', 'slot', 'edit',
  ($scope, $rootScope, $location, $q, $filter, constants, display, messages, Segment, Store, slot, editing) ->
    display.title = slot.displayName
    $scope.flowOptions = Store.flowOptions
    $scope.slot = slot
    $scope.volume = slot.volume
    $scope.editing = editing
    $scope.mode = if editing then 'edit' else 'view'
    $scope.form = {}

    video = undefined
    blank = undefined

    searchLocation = (url) ->
      url
        .search('asset', undefined)
        .search('record', undefined)
        .search($scope.current?.type || '', $scope.current?.id)

    if editing || slot.checkPermission(constants.permission.EDIT)
      url = if editing then slot.route else slot.editRoute()
      display.toolbarLinks.push
        type: 'yellow'
        html: constants.message(if editing then 'slot.view' else 'slot.edit')
        #url: url
        click: -> searchLocation($location.url(url))

    byId = (a, b) -> a.id - b.id
    byPosition = (a, b) -> a.segment.l - b.segment.l

    updateRange = (segment) ->
      if isFinite(slot.segment.l)
        $scope.range.l = slot.segment.l
      else if isFinite(segment.l) && segment.l < $scope.range.l
        $scope.range.l = segment.l
      
      if isFinite(slot.segment.u)
        $scope.range.u = slot.segment.u
      else if isFinite(segment.u) && segment.u > $scope.range.u
        $scope.range.u = segment.u

    offsetPosition = (offset) ->
      return offset unless isFinite offset
      (offset - $scope.range.base) / ($scope.range.u - $scope.range.l)

    positionOffset = (position) ->
      tl = $('#slot-timeline')
      p = (position - tl.offset().left) / tl.outerWidth()
      if p >= 0 && p <= 1
        $scope.range.l + p * ($scope.range.u - $scope.range.l)

    $scope.positionStyle = (p) ->
      styles = {}
      return styles unless p?
      if p instanceof Segment
        l = offsetPosition(p.l)
        if l < 0
          styles.left = '0px'
          styles['border-left'] = '0px'
          styles['border-top-left-radius'] = '0px'
          styles['border-bottom-left-radius'] = '0px'
        else if l <= 1
          styles.left = 100*l + '%'
        r = offsetPosition(p.u)
        if r > 1
          styles.right = '0px'
          styles['border-right'] = '0px'
          styles['border-top-right-radius'] = '0px'
          styles['border-bottom-right-radius'] = '0px'
        else if r >= 0
          styles.right = 100 - 100*r + '%'
      else
        p = offsetPosition(p)
        if p >= 0 && p <= 1
          styles.left = 100*p + '%'
      styles

    seekOffset = (o) ->
      if video && $scope.current?.asset?.segment.contains(o) && isFinite($scope.current.asset.segment.l)
        video[0].currentTime = (o - $scope.current.asset.segment.l) / 1000
      $scope.position = o

    $scope.seekPosition = (pos) ->
      if o = positionOffset(pos)
        seekOffset(o)
      return

    $scope.play = ->
      if video
        video[0].play()
      else
        $scope.playing = 1
      return

    $scope.pause = ->
      if video
        video[0].pause()
      else
        $scope.playing = 0
      return

    sortTracks = ->
      return unless $scope.tracks
      $scope.tracks.sort (a, b) ->
        if a.asset && b.asset
          isFinite(b.asset.segment.l) - isFinite(a.asset.segment.l) ||
            a.asset.segment.l - b.asset.segment.l ||
            a.asset.segment.u - b.asset.segment.u ||
            a.id - b.id
        else
          !a.asset - !b.asset || !a.file - !b.file

    targetAsset = $location.search().asset
    targetRecord = $location.search().record

    confirmDirty = ->
      not (editing && $scope.current && $scope.form.edit &&
        ($scope.current.dirty = $scope.form.edit.$dirty)) or
          confirm(constants.message('navigation.confirmation'))

    select = (c) ->
      return false if c && !confirmDirty()

      $scope.current = c
      searchLocation($location)
      targetAsset = undefined
      targetRecord = undefined

      $scope.playing = 0
      true

    $scope.dblclick = (c) ->
      range = c.segment
      $scope.selection = range
      if range && isFinite(range.l) && !range.contains($scope.position)
        seekOffset(range.l)
      return

    $scope.click = (event, c) ->
      if !c || $scope.current == c
        $scope.seekPosition event.clientX
      else
        select(c)

    $scope.drag = (down, up, c) ->
      return if c && $scope.current != c

      startPos = down.position ?= positionOffset(down.clientX)
      endPos = positionOffset(up.clientX)
      $scope.selection =
        if startPos <= endPos
          new Segment(startPos, endPos)
        else
          new Segment(endPos, startPos)
      return

    removed = (track) ->
      return if track.asset || track.file || track == blank
      select() if track == $scope.current
      $scope.tracks.remove(track)

    addBlank = () ->
      $scope.tracks.push(blank = new Track())

    class Track extends Store
      constructor: (asset) ->
        super slot, asset

      type: 'asset'

      setAsset: (asset) ->
        super asset
        return unless asset
        updateRange(asset.segment)
        select(this) if `asset.id == targetAsset`

      Object.defineProperty @prototype, 'segment',
        get: -> @asset?.segment

      Object.defineProperty @prototype, 'id',
        get: -> @asset?.id

      positionStyle: ->
        $scope.positionStyle @asset?.segment

      remove: ->
        r = super()
        return removed this unless r?.then
        r.then (done) =>
          removed this if done
        return

      save: ->
        super().then (done) =>
          return unless done
          delete @dirty
          $scope.form.edit.$setPristine() if this == $scope.current
          sortTracks()
        return

      upload: (file) ->
        addBlank() if this == blank
        super(file).then (done) =>
          return removed this unless done
          ### jshint ignore:start ###
          @data.name ||= file.file.name
          ### jshint ignore:end ###
          return
        return

    $scope.fileAdded = (file) ->
      (!$scope.current?.file && $scope.current || blank).upload(file) if editing
      return

    $scope.fileSuccess = Store.fileSuccess
    $scope.fileProgress = Store.fileProgress

    videoEvents =
      pause: ->
        $scope.playing = 0
      playing: ->
        $scope.playing = 1
      ratechange: ->
        $scope.playing = video[0].playbackRate
      timeupdate: ->
        if $scope.current?.asset && isFinite($scope.current.asset.segment.l)
          $scope.position = $scope.current.asset.segment.l + 1000*video[0].currentTime
          if $scope.selection.uBounded && $scope.position >= $scope.selection.u
            video[0].pause()
            seekOffset($scope.selection.l) if $scope.selection.lBounded
      ended: ->
        $scope.playing = 0
        # look for something else to play?

    for ev, fn of videoEvents
      videoEvents[ev] = $scope.$lift(fn)

    @deregisterVideo = (v) ->
      return unless video == v
      video = undefined
      v.off(videoEvents)

    @registerVideo = (v) ->
      this.deregisterVideo video if video
      video = v
      seekOffset($scope.position)
      v.on(videoEvents)

    class Record
      constructor: (r) ->
        @record = slot.volume.records[r.id]
        @segment = Segment.make(r.segment)
        for f in ['age'] when f of r
          @[f] = r[f]
        updateRange @segment
        if editing
          @fillData()

      type: 'record'

      fillData: ->
        @data =
          measures: angular.extend({}, @record.measures)
          position:
            lower: if @segment.lBounded then $filter('timecode')(@segment.l, true)
            upper: if @segment.uBounded then $filter('timecode')(@segment.u, true)

      Object.defineProperty @prototype, 'id',
        get: -> @record.id

      remove: ->
        slot.removeRecord(@record, @segment).then(=>
            records.remove(this)
            select() if $scope.current == this
            placeRecords()
          , (res) =>
            messages.addError
              type: 'red'
              body: 'Unable to remove'
              report: res
          )
        return

      ### jshint ignore:start #### fixed in jshint 2.5.7
      metrics: ->
        ident = constants.category[@record.category]?.ident || [constants.metricName.ident.id]
        (constants.metric[m] for m of @record.measures when !(+m in ident)).sort(byId)

      addMetric = {id:'',name:'Add new value...'}
      addOptions: ->
        metrics = (metric for m, metric of constants.metric when !(m of @data.measures)).sort(byId)
        metrics.unshift addMetric
        metrics
      ### jshint ignore:end ###

      add: ->
        @data.measures[@data.add] = '' if @data.add
        @data.add = ''
        return

      save: ->
        saves = []
        if @form.measures.$dirty
          saves.push @record.save({measures:@data.measures}).then () =>
            @form.measures.$setPristine()
        if @form.position.$dirty
          saves.push slot.moveRecord(@record, @segment, @data.position).then (r) =>
            @form.position.$setPristine()
            return unless 'container' of r # nothing happened
            @segment = Segment.make(r.segment)
            if @segment.empty
              records.remove(this)
              select() if this == $scope.current
            placeRecords()
        $q.all(saves).then(=>
            @fillData()
            delete @dirty
            $scope.form.edit.$setPristine() if this == $scope.current
          , (res) =>
            messages.addError
              type: 'red'
              body: 'Error saving'
              report: res
          )
        return

    placeRecords = () ->
      records.sort (a, b) ->
        a.record.category - b.record.category || a.record.id - b.record.id
      t = []
      overlaps = (rr) -> rr.record.id != r.record.id && s.overlaps(rr.segment)
      for r in records
        s = r.segment
        for o, i in t
          break unless o[0].record.category != r.record.category || o.some(overlaps)
        t[i] = [] unless i of t
        t[i].push(r)
        select(r) if `r.id == targetRecord`
      for r in t
        r.sort byPosition
      $scope.records = t

    $scope.positionBackgroundStyle = (l, i) ->
      $scope.positionStyle(new Segment(l[i].segment.l, if i+1 of l then l[i+1].segment.l else Infinity))

    class Consent
      constructor: (c) ->
        if typeof c == 'object'
          @consent = c.consent
          @segment = Segment.make(c.segment)
        else
          @consent = c
          @segment = Segment.full

      type: 'consent'

      classes: ->
        cn = constants.consent[@consent]
        cls = [cn, 'hint-consent-' + cn]
        cls.push('slot-consent-select') if $scope.current == this
        cls

    $scope.range = new Segment(Infinity, -Infinity)
    # implicitly initialize from slot.segment
    updateRange(Segment.full)
    $scope.selection = Segment.empty

    $scope.tracks = (new Track(asset) for asset in slot.assets)
    addBlank() if editing
    sortTracks()

    records = slot.records.map((r) -> new Record(r))
    placeRecords()

    $scope.consents =
      if Array.isArray(consents = slot.consents)
        consents.map((c) -> new Consent(c))
      else if (consents)
        [new Consent(consents)]
      else
        []

    $scope.playing = 0
    $scope.position = undefined

    if editing
      done = $rootScope.$on '$locationChangeStart', (event, url) ->
        return if url.contains(slot.editRoute())
        return display.cancelRouteChange(event) unless confirmDirty()
        done()
])
