#ifndef ORG_H
#define ORG_H

#include "emp/math/Random.hpp"
#include "emp/tools/string_utils.hpp"

class Organism {
    private:
        double coop_prob;
        double points;
        double p_53;
        double mut_rate = 0.002;
        double food_count;
        emp::Ptr<emp::Random> random;

    public:
    Organism(emp::Ptr<emp::Random> _random, double _coop_prob=0.0, double _points=0.0, double _food_count=3.0, double _p_53=0.5) :
        random(_random), coop_prob(_coop_prob), points(_points), food_count(_food_count), p_53(_p_53) {;}
    Organism(const Organism &) = default;
    Organism(Organism &&) = default;

    Organism & operator=(const Organism &) = default;
    Organism & operator=(Organism &&) = default;
    bool operator==(const Organism &other) const { return (this == &other);}
    bool operator!=(const Organism &other) const {return !(*this == other);}

    double getCoopProb() {return coop_prob;}
    double getPoints() {return points;}
    // own methods for food count
    double getFoodCount() {return food_count;}
    void setFoodCount(double _in) {food_count = _in;}
    void addFoodCount(double _in) {food_count += _in;}
    void substractFoodCount(double _in) {food_count -= _in;}
    // own methods for p_53
    double getP53() {return p_53;}
    void setP53(double _in) {p_53 = _in;}
    void addP53(double _in) {p_53 += _in;}
    void substractP53(double _in) {p_53 -= _in;}
    
    void setCoopProb(double _in) {coop_prob = _in;}
    void setPoints(double _in) {points = _in;}
    void addPoints(double _in) {points += _in;}
  

    void mutate() {
        coop_prob += random->GetRandNormal(0.0, mut_rate);
        if(coop_prob < 0) coop_prob = 0;
        else if (coop_prob > 1) coop_prob = 1;
    }
    //should change to something about food count
    emp::Ptr<Organism> checkReproduction() {
        emp::Ptr<Organism> offspring;
        if(points>=1000) {
            offspring = new Organism(*this);
            points -= 1000;
            offspring->mutate();
            offspring->setPoints(0);
        }
        return offspring;
    }

    void Process(double resources) {
        points += resources;
        
    }


};
#endif