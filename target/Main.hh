namespace Main {
  using namespace PureScript;
  using namespace std;

  auto toNumber(const int) -> double;

  auto easeIn(const double) -> double;

  auto timeNow() -> double;

  auto makeWindow(const int, const int, const std::string) -> any;
  auto makeColor(const int, const int, const int, const int) -> any;
  auto makeTextureFromFile(const std::string) -> any;
  auto makeRectangleShape(const double, const double) -> any;

  auto shapeSetTexture(const any&, const any&, const any&) -> any;

  auto transformableSetRotation(const any&, const any&, const double) -> any;
  auto transformableSetScale(const any&, const any&, const double, const double) -> any;
  auto transformableSetOrigin(const any&, const any&, const double, const double) -> any;
  auto transformableMove(const any&, const any&, const double, const double) -> any;

  auto eventIsClosed(const any&) -> any;

  auto windowIsOpen(const any&) -> any;
  auto windowPollEvent(const any&, const any&) -> any;
  auto windowClear(const any&, const any&) -> any;
  auto windowDisplay(const any&) -> any;
  auto windowDraw(const any&, const any&, const any&) -> any;
  auto windowClose(const any&) -> any;

  auto loop(const any&, const any&) -> any;
}