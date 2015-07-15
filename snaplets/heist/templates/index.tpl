<html>
<head><title>crashboard</title>
<style>
.linkButton { 
    background: none;
    border: none;
    color: #0066ff;
    text-decoration: underline;
    cursor: pointer; 
    padding: 0px;
    font-size: 100%;
    margin: 0px;
}

.post {
    margin: 2px;
    padding: 0px;
    width: 100%;
    border: 2px solid black;
}

.postheader {
    border-bottom: 1px solid black;
    padding: 1px;
    margin: 1px;
}

.postdelete {
    float: right;
    width: 50%;
    padding: 0px;
    margin: 0px;
    text-align: right;
}

.postuser {
    font-weight: bold;
    width: 50%;
    padding: 0px;
    font-size: 100%;
}

.postbody {
    margin: 2px;
}

.postpre {
    font-family: sans-serif;
    padding: 0px;
    margin: 0px;
    white-space: pre-wrap;
}

body {
    font-family: sans-serif;
}
</style>
</head>
<body>

<h1>Crashboard</h1>

<h2>Rules of Crashboard</h2>
<p>Do not trust crashboard. Crashboard is insecure.
<!-- Do not taunt Crashboard. -->
Crashboard does not store data. If there are too many posts, delete some.
If you don't like a post, delete it. 
</p>
<a href="#bottom">Jump to bottom</a>
<hr />
<posts>
  <div id="postdiv_${postid}" class="post">
    <div class="postheader">
      <span class="postuser"><postuser /></span>
      <form action="/${postid}/delete" method="post" class="postdelete">
        <input type="submit" class="linkButton" value="delete" />
      </form>
    </div>
    <div class="postbody"><pre class="postpre"><postbody /></pre></div>
  </div>
</posts>
<a id="bottom" />
<hr />
<newpost><form action="post" method="post">
Username: <input type="text" name="name" value="${user}" /><br>
Message: <textarea name="body" rows="8" cols="40"></textarea>
<input type="submit" value="crash it" />
</form></newpost>
</body>

</html>
