'use strict'

app.controller('volume/slot', [
  '$scope', '$location', '$sce', '$q', 'constantService', 'displayService', 'messageService', 'tooltipService', 'styleService', 'storageService', 'Offset', 'Segment', 'Store', 'slot', 'edit',
  ($scope, $location, $sce, $q, constants, display, messages, tooltips, styles, storage, Offset, Segment, Store, slot, editing) ->
    display.title = slot.displayName
    $scope.flowOptions = Store.flowOptions
    $scope.slot = slot
    $scope.volume = slot.volume
    $scope.editing = editing
    $scope.mode = if editing then 'edit' else 'view'
    target = $location.search()
    $scope.form = {}
    fullRange = new Segment(0, 0)
    ruler = $scope.ruler =
      range: if 'range' of target then new Segment(target.range) else fullRange
      selection: if 'select' of target then new Segment(target.select) else Segment.empty
      position: Offset.parse(target.pos)
      zoomed: 'range' of target

    $flow = $scope.$flow # not really in this scope
    video = undefined
    blank = undefined

    searchLocation = (url) ->
      url
        .search('asset', undefined)
        .search('record', undefined)
        .search($scope.current?.type || '', $scope.current?.id)
        .search('select', if !ruler.selection.empty then ruler.selection.format())
        .search('range', if ruler.zoomed then ruler.range.format())

    if editing || slot.checkPermission(constants.permission.EDIT)
      url = if editing then slot.route() else slot.editRoute()
      display.toolbarLinks.push
        type: 'yellow'
        html: constants.message(if editing then 'slot.view' else 'slot.edit')
        #url: url
        click: ->
          searchLocation($location.url(url))
          return

    byId = (a, b) -> a.id - b.id
    byPosition = (a, b) -> a.segment.l - b.segment.l
    finite = (args...) -> args.find(isFinite)

    updateRange = () ->
      l = Infinity
      u = -Infinity
      for t in $scope.tracks.concat(records, $scope.consents)
        l = t.segment.l if isFinite(t.segment?.l) && t.segment.l < l
        u = t.segment.u if isFinite(t.segment?.u) && t.segment.u > u
      fullRange.l = finite(slot.segment.l, l, 0)
      fullRange.u = finite(slot.segment.u, u, 0)
      return

    offsetPosition = (offset) ->
      return offset unless isFinite offset
      (offset - ruler.range.base) / (ruler.range.u - ruler.range.l)

    tl = document.getElementById('slot-timeline')
    positionOffset = (position) ->
      tlr = tl.getBoundingClientRect()
      p = (position - tlr.left) / tlr.width
      if p >= 0 && p <= 1
        ruler.range.l + p * (ruler.range.u - ruler.range.l)
      else if p < 0
        -Infinity
      else if p > 1
        Infinity

    $scope.positionStyle = (p) ->
      style = {}
      return style unless p?
      if p instanceof Segment
        l = offsetPosition(p.l)
        if l < 0
          style.left = '0px'
          style['border-left'] = '0px'
          style['border-top-left-radius'] = '0px'
          style['border-bottom-left-radius'] = '0px'
        else if l <= 1
          style.left = 100*l + '%'
        r = offsetPosition(p.u)
        if r > 1
          style.right = '0px'
          style['border-right'] = '0px'
          style['border-top-right-radius'] = '0px'
          style['border-bottom-right-radius'] = '0px'
        else if r >= 0
          style.right = 100 - 100*r + '%'
      else
        p = offsetPosition(p)
        if p >= 0 && p <= 1
          style.left = 100*p + '%'
      style

    seekOffset = (o) ->
      if video && $scope.asset?.segment.contains(o) && isFinite($scope.asset.segment.l)
        video[0].currentTime = (o - $scope.asset.segment.l) / 1000
      ruler.position = o
      return

    seekPosition = (pos) ->
      if isFinite(o = positionOffset(pos))
        seekOffset(o)
        unless ruler.selection.contains(o) || ruler.selection.u == o # "loose" contains
          ruler.selection = Segment.empty
          finalizeSelection()
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
      return

    stayDirty = (global) ->
      if global || editing && $scope.current && $scope.form.edit && ($scope.current.dirty = $scope.form.edit.$dirty)
        not confirm(constants.message('navigation.confirmation'))

    select = (c) ->
      return false if stayDirty()

      $scope.current = c
      $scope.asset = c.asset if c.type == 'asset'
      searchLocation($location.replace())
      delete target.asset
      delete target.record

      blank.fillData() if c == blank
      $scope.playing = 0
      finalizeSelection()
      true

    $scope.selectAll = (event, c) ->
      ruler.selection = range = new Segment(c.segment)
      finalizeSelection()
      if range && isFinite(range.l) && !range.contains(ruler.position)
        seekOffset(range.l)
      event.stopPropagation()

    $scope.select = (event, c) ->
      if !c || $scope.current == c
        seekPosition event.clientX
      else
        select(c)
      return

    $scope.setSelection = (pos, u) ->
      sel = ruler.selection
      sel = slot.segment if sel.empty
      ruler.selection =
        if u
          new Segment(Math.min(sel.l, pos), pos+0.1)
        else
          new Segment(pos, Math.max(sel.u, pos+0.1))
      finalizeSelection()
      return

    $scope.dragSelection = (down, up, c) ->
      return false if c && $scope.current != c

      startPos = down.position ?= positionOffset(down.clientX)
      endPos = positionOffset(up.clientX)
      ruler.selection =
        if startPos < endPos
          new Segment(startPos, endPos)
        else if startPos > endPos
          new Segment(endPos, startPos)
        else if startPos = endPos
          new Segment(startPos)
        else
          Segment.empty
      finalizeSelection() if up.type != 'mousemove'
      return

    $scope.zoom = (seg) ->
      seg ?= fullRange
      ruler.range = seg
      ruler.zoomed = seg != fullRange
      searchLocation($location.replace())
      return

    removed = (track) ->
      return if track.asset || track.file
      select() if track == $scope.current
      blank = undefined if track == blank
      $scope.tracks.remove(track)
      return

    $scope.updateSelection = finalizeSelection = ->
      $scope.current.editExcerpt() if $scope.current?.excerpts
      for t in $scope.tags
        t.update()
      return

    getSelection = ->
      if ruler.selection.empty then new Segment(ruler.position) else ruler.selection

    $scope.addBlank = () ->
      unless blank
        $scope.tracks.push(blank = new Track())
      select(blank)
      blank

    class Track extends Store
      constructor: (asset) ->
        super slot, asset
        @excerpts = []
        return

      type: 'asset'

      setAsset: (asset) ->
        super asset
        return unless asset
        @segment = new Segment(asset.segment)
        select(this) if `asset.id == target.asset`
        $scope.asset = asset if $scope.current == this
        return

      fillData: ->
        super()
        if !@asset
          @data.position = if ruler.range.uBounded then ruler.range.u else 0

      Object.defineProperty @prototype, 'id',
        get: -> @asset?.id

      remove: ->
        r = super()
        return removed this unless r?.then
        r.then (done) =>
          removed this if done
          return
        return

      save: ->
        shift = @asset?.segment.l
        super().then (done) =>
          return unless done
          delete @dirty
          $scope.form.edit.$setPristine() if this == $scope.current
          shift -= @asset.segment.l
          if shift
            for e in @excerpts
              e.segment.l -= shift
              e.segment.u -= shift
          updateRange()
          sortTracks()
          return

      upload: (file) ->
        blank = undefined if this == blank
        super(file).then (done) =>
          return removed this unless done
          ### jshint ignore:start ###
          @data.name ||= file.file.name
          ### jshint ignore:end ###
          return
        return

      dragMove: (event) ->
        pos = positionOffset(event.clientX)
        return unless isFinite(pos)
        @segment.u = pos + @segment.length
        @segment.l = pos
        if event.type != 'mousemove'
          @data.position = Math.floor(pos)
          $scope.form.edit.$setDirty()
        return

      editExcerpt: () ->
        @excerpt = undefined
        seg = getSelection()
        return if !@asset || !seg || (@segment.full && !seg.full) || !@segment.overlaps(seg) || !@asset.checkPermission(constants.permission.EDIT)
        excerpt = @excerpts.find((e) -> seg.overlaps(e.segment))
        @excerpt =
          if !excerpt
            target: @asset.inSegment(seg)
            on: false
            classification: ''
          else if excerpt.segment.equals(seg)
            target: excerpt
            on: true
            classification: excerpt.excerpt+''
          else
            null
        return

      excerptOptions: () ->
        opts = super()
        opts[@excerpt.classification] = 'prompt' unless @excerpt.classification of opts
        opts

      saveExcerpt: (value) ->
        return unless @excerpt
        if value == ''
          @excerpt.on = true
        else if value == null && @excerpt.classification == ''
          @excerpt.on = false
        else
          messages.clear(this)
          @excerpt.target.setExcerpt(value) #if @excerpt.on then @excerpt.classification else null
            .then (excerpt) =>
                @excerpts.remove(excerpt)
                if 'excerpt' of excerpt
                  @excerpts.push(excerpt)
                else
                  @excerpt.on = false
                  @excerpt.classification = ''
              , (res) =>
                messages.addError
                  type: 'red'
                  body: constants.message('asset.update.error', @name)
                  report: res
                  owner: this

      canRestore: () ->
        Store.removedAsset if editing && this == blank && Store.removedAsset?.volume.id == slot.volume.id

      restore: () ->
        Store.restore(slot).then (a) =>
          @setAsset(a)
          blank = undefined if this == blank
          return

    $scope.fileAdded = (file) ->
      $flow = file.flowObj
      (!$scope.current?.file && $scope.current || $scope.addBlank()).upload(file) if editing
      return

    $scope.fileSuccess = Store.fileSuccess
    $scope.fileProgress = Store.fileProgress

    fillExcerpts = ->
      tracks = {}
      for t in $scope.tracks when t.asset
        tracks[t.asset.id] = t
      for e in slot.excerpts
        t = tracks[e.id]
        t.excerpts.push(e) if t
      return

    viewportHeightStyle = undefined
    viewportImgHeightStyle = undefined
    viewportVideoHeightStyle = undefined
    setViewportHeight = (h) ->
      viewportHeightStyle = styles.set('.player-main-viewport{height:'+h+'px}', viewportHeightStyle)
      viewportImgHeightStyle = styles.set('.player-main-viewport .asset-display img{max-height:'+(h-16)+'px}', viewportImgHeightStyle)
      viewportVideoHeightStyle = styles.set('.player-main-viewport .asset-display video{height:'+(h-16)+'px}', viewportVideoHeightStyle)
      viewportHeight = h
      storage.set('viewport-height', h)

    viewportMinHeight = 120
    viewportHeight = parseInt(storage.get('viewport-height'), 10) || 360
    ### jshint ignore:start ###
    unless viewportHeight >= viewportMinHeight
      viewportHeight = viewportMinHeight
    ### jshint ignore:end ###
    setViewportHeight(viewportHeight)
    viewport = undefined
    $scope.resizePlayer = (down, up) ->
      viewport ?= document.getElementById('player-main-viewport')
      bar = down.currentTarget
      h = Math.max(viewport.offsetHeight + up.clientY - down.clientY, viewportMinHeight)
      if up.type == 'mousemove'
        bar.style.top = h - viewport.offsetHeight + 'px'
      else
        setViewportHeight(h)
        bar.style.top = '0px'
      return

    videoEvents =
      pause: ->
        $scope.playing = 0
        return
      playing: ->
        $scope.playing = 1
        return
      ratechange: ->
        $scope.playing = video[0].playbackRate
        return
      timeupdate: ->
        if $scope.asset && isFinite($scope.asset.segment.l)
          ruler.position = $scope.asset.segment.l + 1000*video[0].currentTime
          if ruler.selection.uBounded && ruler.position >= ruler.selection.u
            video[0].pause()
            seekOffset(ruler.selection.l) if ruler.selection.lBounded
        return
      ended: ->
        $scope.playing = 0
        # look for something else to play?
        return

    for ev, fn of videoEvents
      videoEvents[ev] = $scope.$lift(fn)

    @deregisterVideo = (v) ->
      return unless video == v
      video = undefined
      v.off(videoEvents)
      return

    @registerVideo = (v) ->
      this.deregisterVideo video if video
      video = v
      seekOffset(ruler.position)
      v.on(videoEvents)
      return

    class Record
      constructor: (r) ->
        @rec = r
        @record = r.record || slot.volume.records[r.id]
        for f in ['age'] when f of r
          @[f] = r[f]
        @segment = new Segment(r.segment)
        if editing
          @fillData()
        return

      type: 'record'

      fillData: ->
        @data =
          measures: angular.extend({}, @record.measures)
          add: ''
        return

      Object.defineProperty @prototype, 'id',
        get: -> @rec.id

      remove: ->
        messages.clear(this)
        slot.removeRecord(@rec, @segment).then (r) =>
            return unless r
            records.remove(this)
            select() if $scope.current == this
            placeRecords()
            return
          , (res) ->
            messages.addError
              type: 'red'
              body: 'Unable to remove'
              report: res
              owner: this
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
        messages.clear(this)
        saves = []
        if @form.measures.$dirty
          saves.push @record.save({measures:@data.measures}).then () =>
            @form.measures.$setPristine()
            return
        if @form.position.$dirty
          saves.push slot.moveRecord(@rec, @rec.segment, @segment).then (r) =>
            @form.position.$setPristine()
            return unless r # nothing happened
            @segment = new Segment(r.segment)
            if @segment.empty
              records.remove(this)
              select() if this == $scope.current
            placeRecords()
            updateRange()
            return
        $q.all(saves).then =>
            @fillData()
            delete @dirty
            $scope.form.edit.$setPristine() if this == $scope.current
            return
          , (res) =>
            messages.addError
              type: 'red'
              body: 'Error saving'
              report: res
              owner: this
            return

      dragLeft: (event) ->
        @segment.l = positionOffset(event.clientX)
        if event.type != 'mousemove'
          @form.position.$setDirty()
        return

      dragRight: (event) ->
        @segment.u = positionOffset(event.clientX)
        if event.type != 'mousemove'
          @form.position.$setDirty()
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
        select(r) if `r.id == target.record`
      for r in t
        r.sort byPosition
      $scope.records = t
      return

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
        return

      type: 'consent'

      classes: ->
        cn = constants.consent[@consent]
        cls = [cn, 'hint-consent-' + cn]
        cls.push('slot-consent-select') if $scope.current == this
        cls

    class TagName
      constructor: (name) ->
        @id = name
        return

      save: (vote) ->
        seg = getSelection()
        tag = this
        slot.setTag(@id, vote, editing, seg).then (data) ->
            unless tag instanceof Tag
              tag = $scope.tags.find (t) -> t.id == data.id
              unless tag
                tag = new Tag(data)
                tag.active = true
                $scope.tags.push(tag)
            tag.fillData(data)
            if (if editing then tag.keyword else tag.weight)
              tag.update()
            else
              $scope.tags.remove(tag)
            tooltips.clear() # hack for broken tooltips
            return
          , (res) ->
            messages.addError
              body: constants.message('tags.vote.error', {sce: $sce.HTML}, name)
              report: res
              owner: $scope
            return

    $scope.vote = (name, vote) ->
      new TagName(name).save(vote)

    class Tag extends TagName
      constructor: (t) ->
        @id = t.id
        @active = false
        @fillData(t)
        return

      type: 'tag'

      fillData: (t) ->
        @weight = t.weight
        for f in (if editing then ['keyword'] else ['coverage','vote','keyword'])
          this[f] = []
          if t[f]
            for s in t[f]
              this[f].push(Segment.make(s))
        return

      toggle: ->
        @active = !@active

      update: ->
        state = false
        sel = getSelection()
        for s in (if editing then @keyword else @vote)
          if sel.contains(s)
            state = true
          else if sel.overlaps(s)
            state = undefined
            break
        @state = state

    ### jshint ignore:start #### fixed in jshint 2.5.7
    $scope.tags = (new Tag(tag) for tagId, tag of slot.tags when !editing || tag.keyword)
    $scope.tracks = (new Track(asset) for assetId, asset of slot.assets)
    ### jshint ignore:end ###
    sortTracks()
    fillExcerpts()

    records = slot.records.map((r) -> new Record(r))

    $scope.consents =
      if Array.isArray(consents = slot.consents)
        consents.map((c) -> new Consent(c))
      else if (consents)
        [new Consent(consents)]
      else
        []

    $scope.playing = 0
    placeRecords()
    updateRange()
    finalizeSelection()

    if editing
      done = $scope.$on '$locationChangeStart', (event, url) ->
        return if url.includes(slot.editRoute())
        if $flow
          uploading = $flow.isUploading()
          $flow.pause()
        if stayDirty(uploading)
          $flow.resume() if uploading
          display.cancelRouteChange(event)
        else
          done()
        return

    $scope.$on '$destroy', ->
      $flow?.cancel()
      return

    return
])
