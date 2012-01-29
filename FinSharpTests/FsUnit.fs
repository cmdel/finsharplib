module FsUnit
open NUnit.Framework
open NUnit.Framework.Constraints

GlobalSettings.DefaultFloatingPointTolerance <- 0.0000001

let should (f : 'a -> #Constraint) x (y : obj) =
   let c = f x
   let y =
       match y with
       | :? (unit -> unit) -> box (new TestDelegate(y :?> unit -> unit))
       | _                 -> y
   Assert.That(y, c)

let follows (f : 'a -> #Constraint) x pre transform =
   let y = transform pre
   should f x y

let equal x = new EqualConstraint(x)

let be_less_than x = new LessThanConstraint(x)

let be_greater_than x = new GreaterThanConstraint(x)

let be_less_than_or_equal_to x = new LessThanOrEqualConstraint(x)

let be_greater_than_or_equal_to x = new GreaterThanOrEqualConstraint(x)

let not x = new NotConstraint(x)

let contain x = new ContainsConstraint(x)

let haveLength n = Has.Length.EqualTo(n)

let haveCount n = Has.Count.EqualTo(n)

let be = id

let Null = new NullConstraint()

let Empty = new EmptyConstraint()

let EmptyString = new EmptyStringConstraint()

let NullOrEmptyString = new NullOrEmptyStringConstraint()

let True = new TrueConstraint()

let False = new FalseConstraint()

let sameAs x = new SameAsConstraint(x)

let throw = Throws.TypeOf

// 2.06|> should (equal |> within 0.1) 2.07; or: should (within 0.1 equal) 2.07
let within t (f:float->EqualConstraint) x = (f x).Within(t)

let ranges (lo, hi) values =
    let loActual = Seq.min values
    let hiActual = Seq.max values
    loActual |> should be_greater_than_or_equal_to lo
    hiActual |> should be_less_than_or_equal_to hi
    ()
