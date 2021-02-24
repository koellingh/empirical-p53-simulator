#ifndef WORLD_H
#define WORLD_H

#include "emp/Evolve/World.hpp"
#include "emp/data/DataFile.hpp"
#include "emp/math/random_utils.hpp"
#include "emp/math/Random.hpp"

#include "Org.h"

#include<cmath>

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
    auto & node3 = GetP53DataNode();
    node.SetupBins(0.0, 1.1, 10); //Necessary because range exclusive
    file.AddVar(update, "update", "Update");
    file.AddMean(node, "mean_coopval", "Average organism cooperation value");
    // adding mean to new datamonitor, food_count
    file.AddMean(node2, "mean_foodval", "Average organism food value");
    //added p53 data monitor
    file.AddMean(node3, "mean_p53", "Average organism p53 value");
    file.AddTotal(node1, "count", "Total number of organisms");



    file.PrintHeaderKeys();

    return file;
  }

  // bool isNeighbor(int id1, int id2){
  //     emp_assert(pop_sizes.size() == 2);

  //     int size_x = (int) pop_sizes[0];
  //     int size_y = (int) pop_sizes[1];

  //     int diff = id1 - id2;
  //     int row_diff = abs(diff / size_x);
  //     int col_diff = abs(diff%size_x);

  //     if((row_diff <= 1 || row_diff == (size_y-1)) && 
  //       (col_diff <= 1 || col_diff == (size_x-1)))  return true;
  //     else 
  //     return false;
  // }  
  std::size_t* GetNeighbors(size_t init_pos){
    /*
    100X100 grid
    100s digit floor(init_pos\\100)
    init_pos - 100s digit *100

    (init_pos / 100) <=5 && init_pos % 100 == 0 z
    */
    
    std::size_t y_pos = floor(init_pos/100);
    std::size_t x_pos = init_pos - (y_pos*100);
    int lower_y = y_pos - 3;
    if(lower_y < 0) lower_y = 0;
    int upper_y = y_pos + 3;
    if(upper_y > 99) upper_y = 99;
    int lower_x = x_pos - 3;
    if(lower_x < 0) lower_x = 0;
    int upper_x = x_pos + 3;
    if(upper_x < 99) upper_x = 99;

    size_t neighbor_array [49] = {NULL};
    int spot = 0;
    for(int i = lower_y; i<=upper_y; i++){
      for(int j = lower_x; i<=upper_x; j++){
        size_t index = (i*100) + j;
        neighbor_array[spot] = index;
        spot ++;
        
      }

    }
  //std::cout << neighbor_array;
  return neighbor_array;

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

           if(offspring && offspring->getP53() == -1){
             //kills the pop[i] cell if the offspring value is null
             //std::cout << "actual death wow" <<std::endl;
              DoDeath(i);
           }

          else if(offspring) {
              DoBirth(*offspring, i);
              //emp::WorldPosition rand_neighbor = GetRandomNeighborPos(i);
              //std::cout << "I is: " << i <<std::endl;
              //std::cout << "neighbor is: " << to_string(rand_neighbor) <<std::endl;

              //std::cout << "hmmm birth" << std::endl;
          }
          if(pop[i]->getMutMalig()){
            std::cout<<"malig mutation";
            size_t* neighbors = GetNeighbors(i);
          }


          total_coop += pop[i]->getCoopProb();
          //std::cout << "I is: " << i <<std::endl;

      }
      emp::DataMonitor<double> data_food = GetFoodCountDataNode();
      //std::cout << "Average cooperation: " << total_coop/GetNumOrgs() <<std::endl;
      std::cout << "food left: " << init_food <<std::endl;

  }

};
#endif