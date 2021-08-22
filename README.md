# Erlang_Final_Project
Final project in functional programming in concurrent and distributed systems coruse, Ben-Gurion University of The Negev.
Year: 2021, Semester B.
Creators: Katrin Nekhin, Yuval Narkiss.
Developed in: Ubuntu Version: 20.04 | Erlang Version: 22

Introduction:
This project implements a simulation of a sensor network in a defined area.
The area is equally divided into 4 quarters, each managed by a dedicated erlang node.
All servers(the erlang nodes mentioned above) communicate with each other, and with 'Main PC'(also an erlang node) which holds backup and executes the GUI.
Each sensor's goal is to collect data from its surroundings and send towards a pre-defined stationary destination,
which accumulates data and in turn sends the bulk to the main pc.
The 'Main PC' performs several statistic calculations and presents it in a convinient and readable way on screen.

How To Run The Program:
The system is designed to run distributively. Therefore one may use up to 5 different computers to run the system.
In each physical comupter that will take part of running the system, make sure that all the project files are present and compiled.

1. From a terminal opened in the project's directory(specifically in the 'src' directory), setup an erlang node: "erl -name pcI@IP_ADDRESS_OF_NODE -setcookie COOKIE"
where 1 <= I <= 5, IP_ADDRESS_OF_NODE* is the ip address of the comupter, and COOKIE** is the desired sercert cookie,
and pc5 should be the Main PC. 
*Note that the whole system can run in one physical computer, so for example one could set up TWO erlang nodes at the same physical computer(or even all 5).
**COOKIE should ofcourse be identical in ALL erlang nodes.

2. pc5(the Main PC) should run the following command:
main_PC:start('pc1@IP_ADDRESS_OF_NODE','pc2@IP_ADDRESS_OF_NODE','pc3@IP_ADDRESS_OF_NODE','pc4@IP_ADDRESS_OF_NODE',).

The program should appear on screen of pc5.
