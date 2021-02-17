//  This file is part of Evolutionary Algorithm
//  Copyright (C) Anya Vostinar, 2021.
//  Released under MIT license; see LICENSE

#include <iostream>

#include "emp/base/vector.hpp"
#include "emp/config/command_line.hpp"
#include "emp/math/random_utils.hpp"
#include "emp/math/Random.hpp"

#include "World.h"
#include "emp/config/ArgManager.hpp"


// This is the main function for the NATIVE version of this project.

EMP_BUILD_CONFIG(MyConfigType,
    VALUE(SEED, int, 10, "What value should the random seed be?"), 
    VALUE(START_PROB, double, 0.5, "What cooperation probability value should the starting organism have?"),
    VALUE(FILE_NAME, std::string, "_data.dat", "Root output file name"),
    VALUE(REPRODUCE_FOOD, double, 10, "Amount of food required to reproduce"),
    VALUE(INIT_FOOD, double, 100, "Base amount of food in environment"),
    VALUE(MUT_PROB, double, 0.4, "Chance of mutation"),
    VALUE(MUT_MALIG, double, 0.05, "Chance of developing a MALIG mutation"),
    VALUE(MUT_BENIG, double, 0.05, "Chance of developing a BENIG mutation") , 
    VALUE(DOES_MUTATE, bool, true, "If there should be a mutation"),
    VALUE(INIT_P53, double, 0.5, "Amount of P53 //gene")
)
// config.DOES_MUTATE()



int main(int argc, char* argv[])
{
  MyConfigType config;
  bool success = config.Read("MySettings.cfg");
  if(!success) config.Write("MySettings.cfg");

  auto args = emp::cl::ArgManager(argc, argv);
  if (args.ProcessConfigOptions(config, std::cout, "MySettings.cfg") == false) {
  std::cerr << "There was a problem in processing the options file." << std::endl;
  exit(1);
  }
  if (args.TestUnknown() == false) {
  std::cerr << "Leftover args no good." << std::endl;
  exit(1);
  }
  emp::Random random(2);
  OrgWorld world(random);

  world.SetupOrgFile("Org_Vals_1.dat");
  

  emp::Ptr<Organism> new_org = new Organism(&random, 0.5);
  //passing config variables in
  new_org -> setDoesMutate(config.DOES_MUTATE());
  new_org -> setInitFood(config.INIT_FOOD());
  new_org -> setMutRate(config.MUT_PROB());
  new_org -> setMutMalig(config.MUT_MALIG());
  new_org -> setMutBenig(config.MUT_BENIG());
  new_org -> setP53(config.INIT_P53());
  new_org -> setFoodCount(config.REPRODUCE_FOOD());
  world.Inject(*new_org);
  world.Resize(100,100);
  
  for(int i=0; i<1000; i++) {
    std::cout<< "Update: " << i << std::endl;
    std::cout << "Population Count: " << world.GetNumOrgs() << std::endl;
    world.Update();
  }
}
