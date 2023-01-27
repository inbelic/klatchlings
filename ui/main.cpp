#include <SFML/Graphics.hpp>
#include "erlcomms.h"

#include <atomic>
#include <thread>

std::atomic<bool> running;

void render_main()
{
    running = true;
    sf::RenderWindow window(sf::VideoMode(200, 200), "SFML works!");
    sf::CircleShape shape(100.f);
    shape.setFillColor(sf::Color::Green);

    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            switch (event.type) {
            case sf::Event::Closed:
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
}

int main()
{
    //std::thread render(render_main);

    byte buf[2];
    while (0 < handle_request(buf));

    //render.join();

    return 0;
}
