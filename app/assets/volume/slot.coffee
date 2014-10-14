'use strict'

app.controller('volume/slot', [
  '$scope', 'slot', 'edit', 'pageService', 'Store',
  ($scope, slot, editing, page, Store) ->
    page.display.title = slot.displayName
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

    if editing || slot.checkPermission(page.permission.EDIT)
      url = if editing then slot.route else slot.editRoute()
      page.display.toolbarLinks.push
        type: 'yellow'
        html: page.constants.message(if editing then 'slot.view' else 'slot.edit')
        #url: url
        click: -> searchLocation(page.$location.url(url))


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
      if p instanceof page.models.Segment
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

    $scope.play = ->
      if video
        video[0].play()
      else
        $scope.playing = 1

    $scope.pause = ->
      if video
        video[0].pause()
      else
        $scope.playing = 0

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

    targetAsset = page.$location.search().asset
    targetRecord = page.$location.search().record

    selectRange = (range) ->
      if range && isFinite(range.l) && !range.contains($scope.position)
        seekOffset(range.l)

    confirmDirty = ->
      not (editing && $scope.current && $scope.form.edit &&
        ($scope.current.dirty = $scope.form.edit.$dirty)) or
          confirm(page.constants.message('navigation.confirmation'))

    select = (c, event) ->
      if $scope.current == c
        $scope.seekPosition event.clientX if event
        return

      return if c && !confirmDirty()

      $scope.current = c
      searchLocation(page.$location)
      targetAsset = undefined
      targetRecord = undefined

      $scope.playing = 0
      return unless c
      selectRange(c.segment)

    removed = (track) ->
      return if track.asset || track.file || track == blank
      select() if track == $scope.current
      $scope.tracks.remove(track)

    class Track extends Store
      constructor: (asset) ->
        super slot, asset

      type: 'asset'

      setAsset: (asset) ->
        super asset
        return unless asset
        updateRange(asset.segment)
        @select() if `asset.id == targetAsset`

      Object.defineProperty @prototype, 'segment',
        get: -> @asset?.segment

      Object.defineProperty @prototype, 'id',
        get: -> @asset?.id

      select: (event) ->
        select this, event

      positionStyle: ->
        $scope.positionStyle @asset?.segment

      remove: ->
        r = super()
        return removed this unless r?.then
        r.then (done) =>
          removed this if done

      save: ->
        super().then (done) =>
          return unless done
          delete @dirty
          $scope.form.edit.$setPristine() if this == $scope.current
          sortTracks()

      upload: (file) ->
        super(file)?.then (done) =>
          return unless done
          ### jshint ignore:start ###
          @data.name ||= file.file.name
          ### jshint ignore:end ###
          $scope.tracks.push(blank = new Track()) if this == blank

    $scope.fileAdded = (file) ->
      return unless editing
      (!$scope.current?.file && $scope.current || blank).upload(file)

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
        @segment = page.models.Segment.make(r.segment)
        for f in ['age'] when f of r
          @[f] = r[f]
        updateRange @segment
        if editing
          @fillData()

      type: 'record'

      fillData: ->
        @data = angular.extend({}, @record.measures)

      Object.defineProperty @prototype, 'id',
        get: -> @record.id

      remove: ->
        select() if $scope.current == this
        slot.removeRecord(@record).then =>
          for t in $scope.records
            if t.remove(this)
              if !t.length
                $scope.records.remove(t)
              break

      select: (event) ->
        select this, event

      metrics: ->
        ident = page.constants.category[@record.category]?.ident || [page.constants.metricName.ident.id]
        metrics = []
        for m of @record.measures
          m = +m
          metrics.push(m) unless m in ident
        metrics.sort (a, b) -> a - b

      addOptions: ->
        metric for m, metric of page.constants.metric when !(m of @data)

      add: ->
        @data[@data.add] = ''
        delete @data.add

      save: ->
        @record.save({measures:@data}).then () =>
          @fillData()
          delete @dirty
          $scope.form.edit.$setPristine() if this == $scope.current

    class Consent
      constructor: (c) ->
        if typeof c == 'object'
          @consent = c.consent
          @segment = page.models.Segment.make(c.segment)
        else
          @consent = c
          @segment = page.models.Segment.full

      type: 'consent'

      select: (event) ->
        select this, event

      classes: ->
        cn = page.constants.consent[@consent]
        cls = [cn, 'hint-consent-' + cn]
        cls.push('slot-consent-select') if $scope.current == this
        cls

    $scope.range = new page.models.Segment(Infinity, -Infinity)
    # implicitly initialize from slot.segment
    updateRange(page.models.Segment.full)

    $scope.tracks = (new Track(asset) for asset in slot.assets)
    $scope.tracks.push(blank = new Track()) if editing
    sortTracks()

    $scope.records = (->
      records = slot.records.map((r) -> new Record(r))
      records.sort (a, b) ->
        a.record.category - b.record.category || a.record.id - b.record.id

      t = []
      overlaps = (r) -> s.overlaps(r.segment)
      for r in records
        s = r.segment
        for o, i in t
          break unless o[0].record.category != r.record.category || o.some(overlaps)
        t[i] = [] unless i of t
        t[i].push(r)
        r.select() if `r.id == targetRecord`
      t
    )()

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
      done = page.$rootScope.$on '$locationChangeStart', (event, url) ->
        return if url.contains(slot.editRoute())
        return event.preventDefault() unless confirmDirty()
        done()
])
