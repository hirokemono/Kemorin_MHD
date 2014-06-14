!
!   module   m_2nd_geometry_param
!>   number of node, element, surface, and edge data
!>   and SMP stack for second mesh
!
!   Written by H. Matsui on Aug., 2006
!
!      subroutine allocate_2nd_geomet_param_smp
!      subroutine deallocate_2nd_geomet_param_smp
!
!      subroutine check_smp_size_2nd(my_rank)
!
!
      module m_2nd_geometry_param
!
      use m_precision
!
      implicit  none
!
!>     number of node on local PE (include external node)
      integer( kind=kint )  ::  nnod_2nd
!>     number of node on local PE
      integer( kind=kint )  ::  internal_nod_2nd
!>     number of element on local PE
      integer( kind=kint )  ::  nele_2nd
!>     number of internal element on local PE
      integer( kind=kint )  ::  internal_ele_2nd
!
!>   number of nodes in each element
      integer(kind=kint) :: nnod_4_ele_2nd =   8
!
!
!>     smp stack for total node on  local PE
      integer( kind=kint ), pointer :: inod_smp_stack_2nd(:)
!>     smp stack for internal node on  local PE
      integer( kind=kint ), pointer :: inter_smp_stack_2nd(:)
!>     maximum number of smp node on local PE
      integer( kind=kint )  ::  maxnod_4_smp_2nd = 0
!>     maximum number of smp internal node on local PE
      integer( kind=kint )  ::  max_in_nod_4_smp_2nd = 0
!
!>     smp stack for element on  local PE
      integer( kind=kint ), pointer :: iele_smp_stack_2nd(:)
!>     maximum number of smp element on local PE
      integer( kind=kint )  ::  maxele_4_smp_2nd = 0
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
       subroutine allocate_2nd_geomet_param_smp
!
       use m_machine_parameter
!
       allocate( iele_smp_stack_2nd(0:np_smp))
       allocate( inod_smp_stack_2nd(0:np_smp))
       allocate( inter_smp_stack_2nd(0:np_smp))
!
       iele_smp_stack_2nd = 0
       inod_smp_stack_2nd = 0
       inter_smp_stack_2nd = 0
!
       end subroutine allocate_2nd_geomet_param_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine deallocate_2nd_geomet_param_smp
!
       deallocate( iele_smp_stack_2nd)
       deallocate( inod_smp_stack_2nd)
       deallocate( inter_smp_stack_2nd)
!
       end subroutine deallocate_2nd_geomet_param_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_smp_size_2nd(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inod_smp_stack_2nd ', inod_smp_stack_2nd
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inter_smp_stack_2nd ', inter_smp_stack_2nd
       write(*,*) 'PE: ', my_rank,                                      &
     &           'iele_smp_stack_2nd ', iele_smp_stack_2nd
!
      end subroutine check_smp_size_2nd
!
!-----------------------------------------------------------------------
!
      end module m_2nd_geometry_param
