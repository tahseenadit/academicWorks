#ifndef lacpp_sorted_list_hpp
#define lacpp_sorted_list_hpp lacpp_sorted_list_hpp

/* a sorted list implementation by David Klaftenegger, 2015
 * please report bugs or suggest improvements to david.klaftenegger@it.uu.se
 */

/* struct for list nodes */
template<typename T>
struct node {
	T value;
	node<T>* next;
        std::mutex mutex;
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
			current->mutex.lock();
			current->value = v;
        
			/* insert new node between pred and succ */
			current->next = succ;
			if(pred == nullptr) {
				first = current;
			} else {
				pred->next = current;
			}
		        current->mutex.unlock();
		}

		void remove(T v) {
		  //mutex.lock();
			/* first find position */
			node<T>* pred = nullptr;
			node<T>* current = first;
			current->mutex.lock();
			while(current != nullptr && current->value < v) {
				pred = current;
				current->mutex.unlock();
				current = current->next;
				current->mutex.lock();
			}
			if(current == nullptr || current->value != v) {
				/* v not found */
			        current->mutex.unlock();
				return;
			}
			/* remove current */
			if(pred == nullptr) {
				first = current->next;
			} else {
				pred->next = current->next;
			}
			delete current;
			current->mutex.unlock();
			//mutex.unlock();
		}

		/* count elements with value v in the list */
		std::size_t count(T v) {
			std::size_t cnt = 0;
			/* first go to value v */
			node<T>* current = first;
			current->mutex.lock();
			while(current != nullptr && current->value < v) {
			        current->mutex.unlock();
				current = current->next;
				current->mutex.lock();
			}
			/* count elements */
			while(current != nullptr && current->value == v) {
				cnt++;
				current->mutex.unlock();
				current = current->next;
				current->mutex.lock();
			}
			current->mutex.unlock();
			return cnt;
		}
              
};

#endif // lacpp_sorted_list_hpp
