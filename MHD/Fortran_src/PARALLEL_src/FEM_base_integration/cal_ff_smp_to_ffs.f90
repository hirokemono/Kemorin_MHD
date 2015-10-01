!cal_ff_smp_to_ffs.f90
!     module cal_ff_smp_to_ffs
!
!     Written by H. Matsui on June, 2005
!     Modified by H. Matsui on March, 2009
!     Modified by H. Matsui on March, 2012
!
!> @brief Assemble element integration data to nodal vector
!
!!      subroutine set_ff_nl_smp_2_ff(numdir)
!!      subroutine cal_ff_smp_2_ff(numdir,ff_smp,ff)
!!      subroutine cal_ff_smp_2_ml(ml, ml_o, ff_smp)
!!      subroutine cal_ff_smp_2_scalar                                  &
!!     &         (ff_smp, ml, ncomp_nod, i_fld, d_nod)
!!      subroutine cal_ff_smp_2_vector                                  &
!!     &         (ff_smp, ml, ncomp_nod, i_fld, d_nod)
!!      subroutine cal_ff_smp_2_tensor                                  &
!!     &         (ff_smp, ml, ncomp_nod, i_fld, d_nod)
!
      module cal_ff_smp_to_ffs
!
      use m_precision
!
      use m_geometry_data
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
      call s_set_ff_nl_smp_2_ff(node1%numnod, np_smp,                   &
     &    node1%max_nod_smp, node1%istack_nod_smp,                      &
     &    rhs_tbl1%node_sort_list_smp, numdir,                          &
     &    ff_smp, ff_nl_smp, ff, ff_nl)
!
      end subroutine set_ff_nl_smp_2_ff
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_ff(numdir, ff_smp, ff)
!
      use cal_rhs_node_order
!
      integer(kind = kint), intent(in) :: numdir
      real(kind = kreal), intent(in)                                    &
     &           :: ff_smp(node1%max_nod_smp,3,np_smp)
!
      real(kind = kreal), intent(inout) :: ff(node1%numnod,3)
!
!
      call s_cal_ff_smp_2_ff (node1%numnod, np_smp, node1%max_nod_smp,  &
     &    node1%istack_nod_smp, rhs_tbl1%node_sort_list_smp,            &
     &    numdir, ff_smp, ff)
!
      end subroutine cal_ff_smp_2_ff
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_ml(ml, ml_o, ff_smp)
!
      use cal_rhs_node_order
!
      real(kind=kreal), intent(in)                                      &
     &         :: ff_smp(node1%max_nod_smp,3,np_smp)
      real(kind=kreal), intent(inout) :: ml(node1%numnod)
      real(kind=kreal), intent(inout) :: ml_o(node1%numnod)
!
!
      call s_cal_ff_smp_2_ml (node1%numnod, np_smp, node1%max_nod_smp,  &
     &    node1%istack_nod_smp, rhs_tbl1%node_sort_list_smp,            &
     &    ml, ml_o, ff_smp)
!
      end subroutine cal_ff_smp_2_ml
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_scalar                                    &
     &         (ff_smp, ml, ncomp_nod, i_fld, d_nod)
!
      use cal_rhs_node_order
!
      real(kind=kreal), intent(in) :: ml(node1%numnod)
      real(kind=kreal), intent(in)                                      &
     &           :: ff_smp(node1%max_nod_smp,3,np_smp)
!
      integer (kind=kint), intent(in) :: ncomp_nod, i_fld
      real(kind=kreal), intent(inout) :: d_nod(node1%numnod,ncomp_nod)
!
!
      call s_cal_ff_smp_2_scalar(node1%numnod, np_smp,                  &
     &    node1%max_nod_smp, node1%istack_nod_smp,                      &
     &    rhs_tbl1%node_sort_list_smp, d_nod(1,i_fld), ff_smp, ml)
!
      end subroutine cal_ff_smp_2_scalar
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_vector                                    &
     &         (ff_smp, ml, ncomp_nod, i_fld, d_nod)
!
      use m_phys_constants
      use cal_rhs_node_order
!
      real(kind=kreal), intent(in) :: ml(node1%numnod)
      real(kind=kreal), intent(in)                                      &
     &                  :: ff_smp(node1%max_nod_smp,n_vector,np_smp)
!
      integer (kind=kint), intent(in) :: ncomp_nod, i_fld
      real(kind=kreal), intent(inout) :: d_nod(node1%numnod,ncomp_nod)
!
!
      call s_cal_ff_smp_2_vector(node1%numnod, np_smp,                  &
     &    node1%max_nod_smp, node1%istack_nod_smp,                      &
     &    rhs_tbl1%node_sort_list_smp, d_nod(1,i_fld), ff_smp, ml)
!
      end subroutine cal_ff_smp_2_vector
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_tensor                                    &
     &         (ff_smp, ml, ncomp_nod, i_fld, d_nod)
!
      use m_phys_constants
      use cal_rhs_node_order
!
      real(kind=kreal), intent(in) :: ml(node1%numnod)
      real(kind=kreal), intent(in)                                      &
     &            :: ff_smp(node1%max_nod_smp,n_sym_tensor,np_smp)
!
      integer (kind=kint), intent(in) :: ncomp_nod, i_fld
      real(kind=kreal), intent(inout) :: d_nod(node1%numnod,ncomp_nod)
!
!
      call s_cal_ff_smp_2_tensor(node1%numnod, np_smp,                  &
     &    node1%max_nod_smp, node1%istack_nod_smp,                      &
     &    rhs_tbl1%node_sort_list_smp, d_nod(1,i_fld), ff_smp, ml)
!
      end subroutine cal_ff_smp_2_tensor
!
! ----------------------------------------------------------------------
!
      end module cal_ff_smp_to_ffs
