-- Test of dictionary search, GM 2008
--
-- lin -> linear, should be miserable
-- bin -> home-made binary tree
-- map -> using maps

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Calendar;                      use Ada.Calendar;
with Ada.Unchecked_Deallocation;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

procedure Dico_Drill is

  -- *** Linear search (dumb!) --

  type Lin_Cell(l: Natural) is record
    key: String(1..l);
    elm: Character; -- dummy, just to do something...
  end record;
  type p_Lin_Cell is access Lin_Cell;

  type Lin_Array is array(Integer range <>) of p_Lin_Cell;
  type p_Lin_Array is access Lin_Array;

  -- *** Home-made binary tree (taken from Zip-Ada) --

  type Dir_node;
  type p_Dir_node is access Dir_node;

  type Dir_node(name_len: Natural) is record
    left, right : p_Dir_node;
    key         : String(1..name_len);
    elm         : Character;
  end record;

  procedure Dispose is new Ada.Unchecked_Deallocation( Dir_node, p_Dir_node );

  package Binary_tree_rebalancing is
    procedure Rebalance( root: in out p_Dir_node );
  end Binary_tree_rebalancing;

  package body Binary_tree_rebalancing is

    -------------------------------------------------------------------
    -- Tree Rebalancing in Optimal Time and Space                    --
    -- QUENTIN F. STOUT and BETTE L. WARREN                          --
    -- Commnnications of the ACM September 1986 Volume 29 Number 9   --
    -------------------------------------------------------------------
    -- Translated by (New) P2Ada v. 15-Nov-2006

    procedure Tree_to_vine( root: p_Dir_node; size: out Integer )
      --  transform the tree with pseudo-root
      --   "root^" into a vine with pseudo-root
      --   node "root^", and store the number of
      --   nodes in "size"
    is
      vine_tail, remainder, temp: p_Dir_node;
    begin
      vine_tail := root;
      remainder := vine_tail.right;
      size := 0;
      while remainder /= null loop
        if remainder.left = null then
          --  move vine-tail down one:
          vine_tail := remainder;
          remainder := remainder.right;
          size := size + 1;
        else
          --  rotate:
          temp := remainder.left;
          remainder.left := temp.right;
          temp.right := remainder;
          remainder := temp;
          vine_tail.right := temp;
        end if;
      end loop;
    end Tree_to_vine;

    procedure Vine_to_tree( root: p_Dir_node; size_given: Integer ) is
      --  convert the vine with "size" nodes and pseudo-root
      --  node "root^" into a balanced tree
      leaf_count: Integer;
      size : Integer:= size_given;

      procedure Compression( root: p_Dir_node; count: Integer ) is
        --  compress "count" spine nodes in the tree with pseudo-root "root^"
        scanner, child: p_Dir_node;
      begin
        scanner := root;
        for i in 1..count loop
          child := scanner.right;
          scanner.right := child.right;
          scanner := scanner.right;
          child.right := scanner.left;
          scanner.left := child;
        end loop;
      end Compression;

      -- Returns n - 2 ** Integer( Float'Floor( log( Float(n) ) / log(2.0) ) )
      -- without Float-Point calculation and rounding errors with too short floats
      function Remove_leading_binary_1( n: Integer ) return Integer is
        x: Integer:= 2**16; -- supposed maximum
      begin
        if n < 1 then
          return n;
        end if;
        while n mod x = n loop
          x:= x / 2;
        end loop;
        return n mod x;
      end Remove_leading_binary_1;

    begin --  Vine_to_tree
      leaf_count := Remove_leading_binary_1(size + 1);
      Compression(root, leaf_count); -- create deepest leaves
      -- use Perfect_leaves instead for a perfectly balanced tree
      size := size - leaf_count;
      while size > 1 loop
        Compression(root, size / 2);
        size := size / 2;
      end loop;
    end Vine_to_tree;

    procedure Rebalance( root: in out p_Dir_node ) is
      --  Rebalance the binary search tree with root "root.all",
      --  with the result also rooted at "root.all".
      --  Uses the Tree_to_vine and Vine_to_tree procedures.
      pseudo_root: p_Dir_node;
      size: Integer;
    begin
      pseudo_root:= new Dir_node(name_len => 0);
      pseudo_root.right := root;
      Tree_to_vine(pseudo_root, size);
      Vine_to_tree(pseudo_root, size);
      root := pseudo_root.right;
      Dispose(pseudo_root);
    end Rebalance;

  end Binary_tree_rebalancing;

  Duplicate_name: exception;

  procedure Insert( name: String;
                    elem: Character;
                    node: in out p_Dir_node ) is
  begin
    if node = null then
      node:= new Dir_node'
        ( (name_len => name'Length,
           left => null, right => null,
           key  => name,
           elm  => elem
           )
        );
    elsif name > node.key then
      Insert( name, elem, node.right );
    elsif name < node.key then
      Insert( name, elem, node.left );
    else
      -- raise Duplicate_name;
      null; -- tolerate duplicates
    end if;
  end Insert;

  function Find(name: String; dico: p_Dir_Node) return Character is
    aux: p_Dir_node:= dico;
  begin
    while aux /= null loop
      if name > aux.key then
        aux:= aux.right;
      elsif name < aux.key then
        aux:= aux.left;
      else  -- file found !
        return aux.elm;
      end if;
    end loop;
    raise Constraint_Error;
  end Find;

  -- *** Maps

  package Maps is new Ada.Containers.Hashed_Maps
         (Ada.Strings.Unbounded.Unbounded_String,
          Character,
          Ada.Strings.Unbounded.Hash,
          equivalent_keys => Ada.Strings.Unbounded."=");

  --------------
  -- The Test --
  --------------

  type Mode_Type is (lin, bin, map);

  procedure Test(mode: Mode_Type) is
    t0,t1,t2: Time;
    f: File_Type;
    s: String(1..100);
    l: Natural;

    dico_name  : constant String:= "EN-US.dic"; -- mozilla
    dico_length: constant:= 20_000; -- max ~63_000;

    lina: p_Lin_Array;
    bina: p_Dir_node:= null;
    mapa: Maps.Map;
    pos: Maps.Cursor;
    suc: Boolean;

    elm: Character;

  begin
    --
    -- 1/ Load dictionary
    --
    t0:= Clock;
    Open(f, In_File, dico_name);
    if mode = lin then
      lina:= new Lin_Array(1..dico_length);
    end if;
    for n in 1..dico_length loop
      Get_Line(f,s,l);
      for i in 1..l loop
        if s(i)='/' then
          l:= i-1;
          exit;
        end if;
      end loop;
      elm:= s(l); -- record last character
      case mode is
        when lin =>
          lina(n):= new Lin_Cell(l);
          lina(n).key:= s(1..l);
          lina(n).elm:= elm;
        when bin =>
          Insert(s(1..l), elm, bina);
          if n mod 256 = 0 then
            Binary_tree_rebalancing.Rebalance(bina);
          end if;
        when map =>
          Maps.Insert(mapa,Ada.Strings.Unbounded.To_Unbounded_String(s(1..l)),elm, pos, suc);
      end case;
    end loop;
    Close(f);
    t1:= Clock;
    --
    -- 2/ Open the dictionary as if any text
    --
    Put_Line(
      "Dictionary: " & dico_name &
      "; size:" & Integer'Image(dico_length) &
      "; store/search mode: " & Mode_Type'Image(mode) );
    New_Line;
    Open(f, In_File, dico_name);
    for n in 1..dico_length loop
      Get_Line(f,s,l);
      for i in 1..l loop
        if s(i)='/' then
          l:= i-1;
          exit;
        end if;
      end loop;
      case mode is
        when lin =>
          for i in 1..n loop
            if lina(i).key = s(1..l) then
              elm:= lina(i).elm;
              exit;
            end if;
          end loop;
        when bin =>
          elm:= Find(s(1..l), bina);
        when map =>
          elm:= Maps.Element(mapa, Ada.Strings.Unbounded.To_Unbounded_String(s(1..l)));
      end case;
      if n > dico_length - 4*80 then
        Put(elm);
      end if;
    end loop;
    Close(f);
    t2:= Clock;
    New_Line;
    Put_Line("Time for loading:   " & Duration'Image(t1-t0));
    Put_Line("Time for searching: " & Duration'Image(t2-t1));
    New_Line;
  end Test;

begin
  for mode in Mode_Type loop
    Test(mode);
  end loop;
end Dico_Drill;