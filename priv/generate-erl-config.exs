#!/usr/bin/env elixir


args = System.argv

if length(args) != 1 do
  IO.puts "usage: generate-erl-config.exs <config>\r\n"
  IO.puts :stderr, "no config file specified."
  System.halt 1
end

file = hd(args)

if ! File.exists? file do
  IO.puts :stderr, "no such file or directory '#{file}'"
  System.halt 2
end

defmodule ConfigFactory do

  def generate(config) do
    {unpack, pack} = Enum.reduce config, {[], []}, fn({k, v}, {unpack, pack}) ->
      ks =  String.split atom_to_binary(k), "."
      {[defunpack(ks, v)|unpack],[defpack(ks, v)|pack]}
    end

    quote do
      defmodule Tinymesh.Config do

        def serialize(config) do
          vsn = case Dict.fetch(config, ["device", "fw_version"]) do
            {:ok, v} -> v
            :error -> nil
          end
          serialize(config, vsn)
        end
        def serialize(config, vsn),  do: serialize(config,  vsn, "")
        def serialize([], vsn, acc), do: {:ok, acc}
        def serialize([{k,v}|rest], vsn, acc) do
          case pack(k, v, vsn) do
            {:ok, buf} ->
              serialize rest, vsn, acc <> buf

            {:error, _} = err ->
              err
          end
        end

        def unserialize(buf),      do: unserialize(buf, String.slice(buf, 75, 4))
        def unserialize(buf, vsn), do: unpack(0, buf, vsn, [])

        defp ataddr(addr, val, size) do
          val = case val do
            v when is_binary(v)  -> v
            v when is_integer(v) -> <<v :: [size(size), unit(8), integer()]>>
          end

          vals = :erlang.binary_to_list val

          match = addr + size
          {^match, buf} = Enum.reduce vals, {addr, ""}, fn(v, {p, acc}) ->
            {p+1, acc <> <<p, v>>}
          end
          buf
        end


        def pack(config, vsn), do: pack(config, vsn, "")
        unquote_splicing(pack)
        def pack(key, val, vsn), do:
          {:error, [Enum.join(key, "."),
                    "no such parameter or not applicable version #{vsn}"]}

        def unpack(buf, vsn), do: unpack(0, buf, vsn, [])
        def unpack(_, "", _, acc), do: {:ok, acc}
        unquote_splicing(unpack)
        def unpack(p, <<_ :: [size(1), unit(8)], rest :: binary()>>, fw, acc), do:
          unpack(p+1, rest, fw, acc)
      end
    end
  end

  defp defunpack(key, t) do
    cond do
      t[:type] == :binary or t[:type] == :vsn ->
        quote do
          def unpack(unquote(t[:addr]),
                     <<val :: [size(unquote(t[:size] || 1)), unit(8), unquote(t[:endian] || :big)(), binary()], rest :: binary>>,
                     fw,
                     acc) when fw >= unquote(t[:since]) do
            unpack(unquote(t[:addr] + (t[:size] || 1)), rest, fw, [{unquote(key), val} | acc])
          end
        end

      true ->
        quote do
          def unpack(unquote(t[:addr]),
                     <<val :: [size(unquote(t[:size] || 1)), unit(8), unquote(t[:endian] || :big)()], rest :: binary>>,
                     fw,
                     acc) when fw >= unquote(t[:since]) do
            unpack(unquote(t[:addr] + (t[:size] || 1)), rest, fw, [{unquote(key), val} | acc])
          end
        end
    end
  end

  defp defpack(key, t) do
    try do
      vsnpredA = t[:since]
      vsnpredB = t[:before]
      static  = t[:static]
      type    = t[:type]
      addr    = t[:addr]
      size    = t[:size] || 1

      cond do
        nil !== (t[:enum] || t[:range]) ->
          {match, err} = case {t[:enum], t[:range]} do
            {enums, nil} ->
              {enums, "value should be one of #{Enum.join(enums, ", ")}"}
            {nil, range} ->
              {range, "value must be in range #{Macro.to_string(range)}"}
          end

          case {vsnpredA, vsnpredB} do
            {nil, nil} ->
              quote do
                def pack(unquote(key), val, _) when val in unquote(match), do:
                 {:ok, ataddr(unquote(addr), val, unquote(size))}

                def pack(unquote(key), val, _), do:
                  {:error, [unquote(Enum.join(key, ".")), unquote(err)]}
              end

            {vsnpred, nil} ->
              quote do
                def pack(unquote(key), val, fw) when
                    fw >= unquote(vsnpred) and val in unquote(match), do:

                  {:ok, ataddr(unquote(addr), val, unquote(size))}

                def pack(unquote(key), val, fw)
                    when fw >= unquote(vsnpred) and not val in unquote(match), do:

                  {:error, [unquote(Enum.join(key, ".")), unquote(err)]}
              end

            {nil, vsnpred} ->
              quote do
                def pack(unquote(key), val, fw) when
                    fw < unquote(vsnpred) and val in unquote(match), do:

                  {:ok, ataddr(unquote(addr), val, unquote(size))}

                def pack(unquote(key), val, fw) when
                    fw < unquote(vsnpred) and not val in unquote(match), do:

                  {:error, [unquote(Enum.join(key, ".")), unquote(err)]}
              end
          end

        nil !== static ->
          quote do
            def pack(unquote(key), val, _) when val === unquote(static), do:
              {:ok, ""} # don't return anything, just static
            def pack(unquote(key), val, _) do
              {:error, [unquote(Enum.join(key, ".")),
                        "value must equal #{unquote(static)}"]}
            end
          end

        true === t[:ro] ->
          quote do
            def pack(unquote(key), _, _), do:
              {:error, [unquote(Enum.join(key, ".")), "read-only variable"]}
          end

        :binary === type ->
          quote do
            def pack(unquote(key), val, _) when is_binary(val), do:
              {:ok, ataddr(unquote(addr), val, unquote(size))}
            def pack(unquote(key), _val, _), do:
              {:error, [unquote(Enum.join(key, ".")), "value must be binary"]}
          end

        :vsn === type ->
          quote do
            def pack(unquote(key), <<_, ".", _, _>> = val, _), do:
              {:ok, ""}
            def pack(unquote(key), _val, _), do:
              {:error, [unquote(Enum.join(key, ".")), "value must be in format `x.yz`"]}
          end

        true ->
          quote(do: def pack(unquote(key), val, _) do
              {:ok, ataddr(unquote(addr), val, unquote(size))}
          end)
      end
    rescue e in [ArgumentError]->
      {:error, [Enum.join(key, "."), e]}
    end
  end
end

{config, []} = Code.eval_string File.read! file
quotedconf = ConfigFactory.generate(config)
[{_, bytes}] = Code.compile_quoted quotedconf

bincfg = <<9,5,5,0,190,193,2,255,6,20,30,5,10,20,1,0,1,1,1,1,1,1,1,1,
           0,0,0,0,3,0,0,0,10,7,255,0,0,10,7,255,0,0,10,5,0,2,1,0,0,
           1,0,0,0,5,8,0,1,0,49,4,82,67,49,49,55,48,45,84,77,44,50,
           46,48,48,44,49,46,51,53,255,255,0,0,0,0,255,5,0,0,0,0,1,0,
           0,0,0,1,0,0,10,60,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>

{:ok, cfg} = Tinymesh.Config.unserialize bincfg
Enum.each cfg, &IO.inspect/1

## Do some basic tests to make sure it's not all to crazy
#{:ok, <<0,1>>}                     = Tinymesh.Config.serialize [{["rf", "channel"], 1}]
#{:ok, <<1,5>>}                     = Tinymesh.Config.serialize [{["rf", "power"], 5}]
#{:error, ["rf.power", _]}          = Tinymesh.Config.serialize [{["rf", "power"], 6}]
#{:ok, <<3,1>>}                     = Tinymesh.Config.serialize [{["protocol_mode"], 1}]
#{:error, ["protocol_mode", _]}     = Tinymesh.Config.serialize [{["protocol_mode"], 2}]
#
#{:ok, <<107,3>>}                   = Tinymesh.Config.serialize [{["ima", "data_field"], 3}], "1.40"
#{:error, ["ima.data_field", _]}    = Tinymesh.Config.serialize [{["ima", "data_field"], 3}], "1.27"
#{:ok, <<108,200>>}                 = Tinymesh.Config.serialize [{["ima", "trig_hold"], 200}], "1.40"
#{:ok, <<14,3>>}                    = Tinymesh.Config.serialize [{["device", "type"], 3}], "1.40"
#{:error, ["device.type", _]}       = Tinymesh.Config.serialize [{["device", "type"], 3}], "1.27"
#{:ok, <<14,2>>}                    = Tinymesh.Config.serialize [{["device", "type"], 2}], "1.27"
#{:error, ["gpio_0.trig.hi", _]}    = Tinymesh.Config.serialize [{["gpio_0", "trig", "hi"], -1}]
#{:error, ["gpio_0.trig.hi", _]}    = Tinymesh.Config.serialize [{["gpio_0", "trig", "hi"], 2048}]
#{:ok, <<33,4,34,1>>}               = Tinymesh.Config.serialize [{["gpio_0", "trig", "hi"], 1025}]
#{:error, ["device.uid", _]}        = Tinymesh.Config.serialize [{["device", "uid"], -1}]
#{:error, ["device.uid", _]}        = Tinymesh.Config.serialize [{["device", "uid"], 4294967296}]
#{:ok, <<45,0,46,1,47,4,48,5>>}     = Tinymesh.Config.serialize [{["device", "uid"], 66565}]
#{:error, ["device.part", _]}       = Tinymesh.Config.serialize [{["device", "part"], "RC1140-TM."}]
#{:error, ["device.fw_version", _]} = Tinymesh.Config.serialize [{["device", "fw_version"], "1.23"}]
#{:ok, <<>>}                        = Tinymesh.Config.serialize [{["force_backup"], 85}]
#{:error, ["non-existing-key", _]}  = Tinymesh.Config.serialize [{["non-existing-key"], 123}]
#
#
#File.mkdir_p! "ebin"
#File.write! "ebin/Elixir.Tinymesh.Config.beam", bytes
