#ifndef WORLD_H
#define WORLD_H

#include "emp/Evolve/World.hpp"
#include "emp/data/DataFile.hpp"
#include "emp/math/random_utils.hpp"
#include "emp/math/Random.hpp"

#include "Org.h"

class OrgWorld : public emp::World<Organism> {

    double resources_per_org = 0.5;
    emp::Random &random;
    emp::Ptr<emp::Random> random_ptr;
    double init_food = 10000000;
    emp::Ptr<emp::DataMonitor<double, emp::data::Histogram>> data_node_orgcoop;
    emp::Ptr<emp::DataMonitor<int>> data_node_orgcount;
    // our own DataMonitor pointer
    emp::Ptr<emp::DataMonitor<double>> data_node_foodcount;emp::Ptr<emp::DataMonitor<double>> data_node_p_53_count;

    public:

    OrgWorld(emp::Random &random) : emp::World<Organism>(random), random(random) {
        random_ptr.New(random);
    }

    ~OrgWorld() {
        if(data_node_orgcoop) data_node_orgcoop.Delete();
        if(data_node_orgcount) data_node_orgcount.Delete();
        if(data_node_foodcount) data_node_foodcount.Delete();
        if(data_node_p_53_count) data_node_p_53_count.Delete();
    }

    emp::World<Organism>::pop_t getPop() {return pop;}

    emp::DataMonitor<int>& GetOrgCountDataNode() {
        if(!data_node_orgcount) {
        data_node_orgcount.New();
        OnUpdate([this](size_t){
            data_node_orgcount -> Reset();
            for (size_t i = 0; i< pop.size(); i++)
            if(IsOccupied(i))
                data_node_orgcount->AddDatum(1);
        });
        }
        return *data_node_orgcount;

    }

    emp::DataMonitor<double, emp::data::Histogram>& GetOrgCoopValDataNode() {
        if (!data_node_orgcoop) {
        data_node_orgcoop.New();
        OnUpdate([this](size_t){
            data_node_orgcoop->Reset();
            for (size_t i = 0; i< pop.size(); i++)
            if (IsOccupied(i))
                data_node_orgcoop->AddDatum(pop[i]->getCoopProb());
        });
        }
        return *data_node_orgcoop;
    }

    emp::DataMonitor<double>& GetP53DataNode() {
        if(!data_node_p_53_count) {
        data_node_p_53_count.New();
        OnUpdate([this](size_t){
            data_node_p_53_count -> Reset();
            for (size_t i = 0; i< pop.size(); i++)
            if(IsOccupied(i))
                data_node_p_53_count->AddDatum(pop[i]->getP53());
        });
        }
        return *data_node_p_53_count;

    }
    emp::DataMonitor<double>& GetFoodCountDataNode() {
        if(!data_node_foodcount) {
        data_node_foodcount.New();
        OnUpdate([this](size_t){
            data_node_foodcount -> Reset();
            for (size_t i = 0; i< pop.size(); i++)
            if(IsOccupied(i))
                data_node_foodcount->AddDatum(pop[i]->getFoodCount());
        });
        }
        return *data_node_foodcount;

    }
    emp::DataFile & SetupOrgFile(const std::string & filename) {
    auto & file = SetupFile(filename);
    auto & node1 = GetOrgCountDataNode();
    auto & node = GetOrgCoopValDataNode();
    auto & node2 = GetFoodCountDataNode();
    node.SetupBins(0.0, 1.1, 10); //Necessary because range exclusive
    file.AddVar(update, "update", "Update");
    file.AddMean(node, "mean_coopval", "Average organism cooperation value");
    // adding mean to new datamonitor, food_count
    file.AddMean(node2, "mean_foodval", "Average organism food value");
    file.AddTotal(node1, "count", "Total number of organisms");



    file.PrintHeaderKeys();

    return file;
  }

  void Update() {
      emp::World<Organism>::Update();
      
      emp::vector<size_t> schedule = emp::GetPermutation(random, GetSize());
      double total_coop = 0;
      for (size_t i: schedule) {
          if (IsOccupied(i) == false) continue; 
          if(init_food > 0 ){
            pop[i]->Process(resources_per_org);
            //init_food -= resources_per_org;
          }
          emp::Ptr<Organism> offspring = pop[i]->checkReproduction();
          if(offspring) {
              DoBirth(*offspring, i);
          }

          total_coop += pop[i]->getCoopProb();

      }
      emp::DataMonitor<double> data_food = GetFoodCountDataNode();
      std::cout << "Average cooperation: " << total_coop/GetNumOrgs() <<std::endl;
      std::cout << "food left: " << init_food <<std::endl;
  }

};
#endif