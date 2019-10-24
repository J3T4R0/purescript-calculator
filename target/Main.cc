#include <iostream>
#include <cmath>
#include <math.h>
#include <chrono>
#include <SFML/Graphics.hpp>
#import "PureScript/PureScript.hh"

namespace Main {
  using namespace PureScript;
  using namespace std;
  using namespace std::chrono;
  using namespace sf;

  auto toNumber(const int x) -> double {
    return (double) x;
  }

  auto timeNow() -> double {
    return (double) duration_cast<std::chrono::milliseconds>(
      system_clock::now().time_since_epoch()
    ).count();
  }

  auto easeIn(const double x) -> double {
    return pow(M_E, x / 5.0) * (1.0 / 20.0);
  }

  auto makeWindow(const int width, const int height, const std::string title) -> any {
    return [=]() -> any {
      auto window = make_managed<sf::RenderWindow>(
        sf::VideoMode(width, height),
        title
      );
      return window;
    };
  }

  auto makeColor(const int r, const int g, const int b, const int a) -> any {
    return [=]() -> any {
      auto color = make_managed<sf::Color>(r, g, b, a);
      return color;
    };
  }

  auto makeTextureFromFile(const std::string path) -> any {
    return [=]() -> any {
      auto texture = make_managed<sf::Texture>();
      texture->loadFromFile(path);
      return texture;
    };
  }

  auto makeRectangleShape(const double width, const double height) -> any {
    return [=]() -> any {
      auto shape = make_managed<sf::RectangleShape>(sf::Vector2f(width, height));
      return shape;
    };
  }

  auto shapeSetTexture(const any& _, const any& s, const any& t) -> any {
    return [=]() -> any {
      auto& shape = cast_managed<sf::Shape>(s);
      auto& texture = cast_managed<sf::Texture>(t);
      shape.setTexture(&texture);
    };
  }

  auto transformableSetRotation(const any& _, const any& t, const double rotation) -> any {
    return [=]() -> any {
      auto& transformable = cast_managed<sf::Shape>(t);
      transformable.setRotation(rotation);
    };
  }

  auto transformableSetScale(const any& _, const any& t, const double scaleX, const double scaleY) -> any {
    return [=]() -> any {
      auto& transformable = cast_managed<sf::Shape>(t);
      transformable.setScale(scaleX, scaleY);
    };
  }

  auto transformableSetOrigin(const any& i, const any& t, const double x, const double y) -> any {
    return [=]() -> any {
      auto& transformable = cast_managed<sf::Shape>(t);
      transformable.setOrigin(x, y);
    };
  }

  auto transformableMove(const any& _, const any& t, const double offsetX, const double offsetY) -> any {
    return [=]() -> any {
      auto& transformable = cast_managed<sf::Shape>(t);
      transformable.move(offsetX, offsetY);
    };
  }

  auto eventIsClosed(const any& e) -> any {
    return [=]() -> any {
      auto& event = cast_managed<sf::Event>(e);
      return event.type == sf::Event::Closed;
    };
  }

  auto windowIsOpen(const any& w) -> any {
    return [=]() -> any {
      auto& window = cast_managed<sf::RenderWindow>(w);
      return window.isOpen();
    };
  }

  auto windowPollEvent(const any& w, const any& f) -> any {
    return [=]() -> any {
      auto& window = cast_managed<sf::RenderWindow>(w);
      auto event = make_managed<sf::Event>();
      while (window.pollEvent(*event)) {
        f(event)();
      }
    };
  }

  auto windowDraw(const any& _, const any& w, const any& d) -> any {
    return [=]() -> any {
      auto& window = cast_managed<sf::RenderWindow>(w);
      auto& drawable = cast_managed<sf::Drawable>(d);
      window.draw(drawable);
    };
  }

  auto windowClear(const any& w, const any& c) -> any {
    return [=]() -> any {
      auto& window = cast_managed<sf::RenderWindow>(w);
      auto& color = cast_managed<sf::Color>(c);
      window.clear(color);
    };
  }

  auto windowDisplay(const any& w) -> any {
    return [=]() -> any {
      auto& window = cast_managed<sf::RenderWindow>(w);
      window.display();
    };
  }

  auto windowClose(const any& w) -> any {
    return [=]() -> any {
      auto& window = cast_managed<sf::RenderWindow>(w);
      window.close();
    };
  }

  auto loop(const any& iteration, const any& input) -> any {
    return [=]() -> any {
      any result = iteration(input)();
      bool cont = result.at("continue");
      while(cont) {
        result = iteration(result)();
        cont = result.at("continue");
      }
    };
  }
}