!cal_ff_smp_to_ffs.f90
!     module cal_ff_smp_to_ffs
!
!     Written by H. Matsui on June, 2005
!     Modified by H. Matsui on March, 2009
!     Modified by H. Matsui on March, 2012
!
!> @brief Assemble element integration data to nodal vector
!
!      subroutine set_ff_nl_smp_2_ff(numdir)
!      subroutine cal_ff_smp_2_ff(numdir,ff_smp,ff)
!      subroutine cal_ff_smp_2_ml(ml, ml_o, ff_smp)
!      subroutine cal_ff_smp_2_scalar(scalar, ff_smp, ml)
!      subroutine cal_ff_smp_2_vector(vector, ff_smp, ml)
!      subroutine cal_ff_smp_2_tensor(vector, ff_t_smp, ml)
!
      module cal_ff_smp_to_ffs
!
      use m_precision
!
      use m_geometry_parameter
      use m_machine_parameter
      use m_phys_constants
      use m_sorted_node
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine set_ff_nl_smp_2_ff(numdir)
!
      use m_finite_element_matrix
      use cal_rhs_node_order
!
      integer(kind = kint), intent(in) :: numdir
!
!
      call s_set_ff_nl_smp_2_ff(numnod, np_smp, maxnod_4_smp,           &
     &    inod_smp_stack, node_sort_list_smp, numdir,                   &
     &    ff_smp, ff_nl_smp, ff, ff_nl)
!
      end subroutine set_ff_nl_smp_2_ff
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_ff (numdir, ff_smp, ff)
!
      use cal_rhs_node_order
!
      integer(kind = kint), intent(in) :: numdir
!
      real(kind = kreal), intent(inout) :: ff(numnod,3)
      real(kind = kreal), intent(in) :: ff_smp(maxnod_4_smp,3,np_smp)
!
!
      call s_cal_ff_smp_2_ff (numnod, np_smp, maxnod_4_smp,             &
     &    inod_smp_stack, node_sort_list_smp, numdir, ff_smp, ff)
!
      end subroutine cal_ff_smp_2_ff
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_ml (ml, ml_o, ff_smp)
!
      use cal_rhs_node_order
!
      real(kind=kreal), intent(in) :: ff_smp(maxnod_4_smp,3,np_smp)
      real(kind=kreal), intent(inout) :: ml(numnod)
      real(kind=kreal), intent(inout) :: ml_o(numnod)
!
!
      call s_cal_ff_smp_2_ml (numnod, np_smp, maxnod_4_smp,             &
     &    inod_smp_stack, node_sort_list_smp, ml, ml_o, ff_smp)
!
      end subroutine cal_ff_smp_2_ml
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_scalar (scalar, ff_smp, ml)
!
      use cal_rhs_node_order
!
      real(kind=kreal), intent(inout) :: scalar(numnod)
      real(kind=kreal), intent(in) :: ml(numnod)
      real(kind=kreal), intent(in) :: ff_smp(maxnod_4_smp,3,np_smp)
!
!
      call s_cal_ff_smp_2_scalar (numnod, np_smp, maxnod_4_smp,         &
     &    inod_smp_stack, node_sort_list_smp, scalar, ff_smp, ml)
!
      end subroutine cal_ff_smp_2_scalar
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_vector(vector, ff_smp, ml)
!
      use m_phys_constants
      use cal_rhs_node_order
!
      real(kind=kreal), intent(in) :: ml(numnod)
      real(kind=kreal), intent(in)                                      &
     &                  :: ff_smp(maxnod_4_smp,n_vector,np_smp)
!
      real(kind=kreal), intent(inout) :: vector(numnod,n_vector)
!
!
      call s_cal_ff_smp_2_vector (numnod, np_smp, maxnod_4_smp,         &
     &    inod_smp_stack, node_sort_list_smp, vector, ff_smp, ml)
!
      end subroutine cal_ff_smp_2_vector
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_tensor(tensor, ff_t_smp, ml)
!
      use m_phys_constants
      use cal_rhs_node_order
!
      real(kind=kreal), intent(in) :: ml(numnod)
      real(kind=kreal), intent(in)                                      &
     &                 :: ff_t_smp(maxnod_4_smp,n_sym_tensor,np_smp)
!
      real(kind=kreal), intent(inout) :: tensor(numnod,n_sym_tensor)
!
!
      call s_cal_ff_smp_2_tensor(numnod, np_smp, maxnod_4_smp,          &
     &    inod_smp_stack, node_sort_list_smp, tensor, ff_t_smp, ml)
!
      end subroutine cal_ff_smp_2_tensor
!
! ----------------------------------------------------------------------
!
      end module cal_ff_smp_to_ffs
