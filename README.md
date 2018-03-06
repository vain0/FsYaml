FsYaml
======
F#用のYAMLライブラリです。
F#の型との親和性が高く、YAMLを直接F#で定義した型にマッピングできます。

背景
----
アプリケーションの設定ファイルとしては、XMLがよく使われます。
しかし、XMLは人間にとって読みにくく、また編集も大変です。
それに対して、YAMLはよく使われている範囲の機能を使っている分には読みやすく、編集も容易です。
そのため、言語間をまたぐようなアプリケーションの設定ファイルには、できればYAMLを採用したいです。

ただし、XMLのDOMのようなオブジェクトモデルや、SAXのようなイベントモデルは、
**アプリケーションの設定ファイル**のための仕組みとしては使いにくいです。
欲しいのはドキュメントモデルでもイベントモデルでもなく、単に設定を保持したオブジェクトなのです。

しかし、よく使われるYAMLの基本的なコレクションであるリストとマップを直接使えても、
それはそれであまりうれしくありません。
リストはいいのですが、マップは、

 * 本当にマップが欲しい場合
 * 固定的なフィールド名と、それに紐づく値のペアを複数持った型が欲しい場合

の2つの場合が考えられます。
後者は確かにマップでも実現できるのですが、F#を考えた場合はレコード型として扱いたいのです。

これを実現するために、FsYamlではYAMLの読み込み時に結果の型を指定します。
例えば、ユーザ名とパスワードを保持するYAMLを読み込みたい場合、

```fsharp
type Config = { UserName: string; Password: string }
```

と言う型を作り、

```fsharp
let config = Yaml.load<Config> str
```

のように読み込みます。
これだけで、以下のようなYAML文字列をパースできます。

```yaml
UserName: bleis-tift
Password: aaa
```