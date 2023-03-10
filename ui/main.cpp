#include <SFML/Graphics.hpp>
#include "erlcomms.h"
#include "game.h"
#include <atomic>
#include <thread>

std::atomic<bool> running, gate;

void render_main()
{
    sf::RenderWindow window(sf::VideoMode(200, 200), "SFML works!");
    sf::CircleShape shape(100.f);
    shape.setFillColor(sf::Color::Green);

    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            switch (event.type) {
            case sf::Event::Closed:
                running = false;
                gate = true;
                break;
            case sf::Event::KeyPressed:
                gate = true;
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
    running = true;
    gate = false;
    std::thread render(render_main);

    GameInternal gi;

    byte buf[2];
    while (running && 0 < handle_request(buf, gate));

    running = false;

    render.join();

    return 0;
}
