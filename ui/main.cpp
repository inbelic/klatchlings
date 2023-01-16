#include <SFML/Graphics.hpp>
#include "erlcomms.h"

int main()
{
    bool running = true;
    int res;
    byte buf[2];

    sf::RenderWindow window(sf::VideoMode(200, 200), "SFML works!");
    sf::CircleShape shape(100.f);
    shape.setFillColor(sf::Color::Green);

    while (window.isOpen())
    {
        sf::Event event;
        while (window.pollEvent(event))
        {
            switch (event.type) {
            case sf::Event::Closed:
                running = false;
                break;
            case sf::Event::MouseButtonPressed:
                if (event.mouseButton.button == sf::Mouse::Left)
                    if (handle_request(buf) <= 0)
                        running = false;
                break;
            default:
                break;
            }

            if (!running)
                window.close();
        }

        window.clear();
        window.draw(shape);
        window.display();
    }

    return 0;
}
