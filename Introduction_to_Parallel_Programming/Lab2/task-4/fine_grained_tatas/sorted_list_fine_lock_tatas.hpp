#ifndef lacpp_sorted_list_hpp
#define lacpp_sorted_list_hpp lacpp_sorted_list_hpp

/* a sorted list implementation by David Klaftenegger, 2015
 * please report bugs or suggest improvements to david.klaftenegger@it.uu.se
 */

class TATASlock {
  std::atomic_bool state = { false };
  public:
    void lock(){
      while(1){
        while(state.load()){}
        if(!state.exchange(true)){
          return;
        }
      }
    }
    void unlock(){
      state.exchange(false);
    }
};

/* struct for list nodes */
template<typename T>
struct node {
	T value;
	node<T>* next;
        TATASlock tatas;
};

/* non-concurrent sorted singly-linked list */
template<typename T>
class sorted_list {
	node<T>* first = nullptr;
        
	public:
		/* default implementations:
		 * default constructor
		 * copy constructor (note: shallow copy)
		 * move constructor
		 * copy assignment operator (note: shallow copy)
		 * move assignment operator
		 *
		 * The first is required due to the others,
		 * which are explicitly listed due to the rule of five.
		 */
                
		sorted_list() = default;
		sorted_list(const sorted_list<T>& other) = default;
		sorted_list(sorted_list<T>&& other) = default;
		sorted_list<T>& operator=(const sorted_list<T>& other) = default;
		sorted_list<T>& operator=(sorted_list<T>&& other) = default;
		~sorted_list() {
			while(first != nullptr) {
				remove(first->value);
			}
		}
                
		/* insert v into the list */
		void insert(T v) {
		   
			/* first find position */
			node<T>* pred = nullptr;
			node<T>* succ = first;
	  
			while(succ != nullptr && succ->value < v) {
			        pred = succ;
				succ = succ->next;
			}
			
			/* construct new node */
			node<T>* current = new node<T>();
			current->tatas.lock();
			current->value = v;
        
			/* insert new node between pred and succ */
			current->next = succ;
			if(pred == nullptr) {
				first = current;
			} else {
				pred->next = current;
			}
		        current->tatas.unlock();
		}

		void remove(T v) {
		  //mutex.lock();
			/* first find position */
			node<T>* pred = nullptr;
			node<T>* current = first;
			current->tatas.lock();
			while(current != nullptr && current->value < v) {
				pred = current;
				current->tatas.unlock();
				current = current->next;
				current->tatas.lock();
			}
			if(current == nullptr || current->value != v) {
				/* v not found */
			        current->tatas.unlock();
				return;
			}
			/* remove current */
			if(pred == nullptr) {
				first = current->next;
			} else {
				pred->next = current->next;
			}
			delete current;
			current->tatas.unlock();
			//mutex.unlock();
		}

		/* count elements with value v in the list */
		std::size_t count(T v) {
			std::size_t cnt = 0;
			/* first go to value v */
			node<T>* current = first;
			current->tatas.lock();
			while(current != nullptr && current->value < v) {
			        current->tatas.unlock();
				current = current->next;
				current->tatas.lock();
			}
			/* count elements */
			while(current != nullptr && current->value == v) {
				cnt++;
				current->tatas.unlock();
				current = current->next;
				current->tatas.lock();
			}
			current->tatas.unlock();
			return cnt;
		}
              
};

#endif // lacpp_sorted_list_hpp
