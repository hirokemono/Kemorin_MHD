!cal_ff_smp_to_ffs.f90
!     module cal_ff_smp_to_ffs
!
!     Written by H. Matsui on June, 2005
!     Modified by H. Matsui on March, 2009
!     Modified by H. Matsui on March, 2012
!
!> @brief Assemble element integration data to nodal vector
!
!!      subroutine cal_ff_smp_2_ml_type(node, rhs_tbl, ff_smp, lump)
!!      subroutine set_ff_nl_smp_2_ff(node, rhs_tbl, numdir)
!!
!!      subroutine cal_ff_smp_2_ff(node, rhs_tbl, numdir, ff_smp, ff)
!!      subroutine cal_ff_smp_2_ml(node, rhs_tbl, ml, ml_o, ff_smp)
!!      subroutine cal_ff_smp_2_scalar                                  &
!!     &         (node, rhs_tbl, ff_smp, ml, ncomp_nod, i_fld, d_nod)
!!      subroutine cal_ff_smp_2_vector                                  &
!!     &         (node, rhs_tbl, ff_smp, ml, ncomp_nod, i_fld, d_nod)
!!      subroutine cal_ff_smp_2_tensor                                  &
!!     &         (node, rhs_tbl, ff_smp, ml, ncomp_nod, i_fld, d_nod)
!!        type(node_data), intent(in) :: node
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(inout) :: lump
!
      module cal_ff_smp_to_ffs
!
      use m_precision
!
      use t_geometry_data
      use t_table_FEM_const
      use m_machine_parameter
      use m_phys_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_ml_type(node, rhs_tbl, ff_smp, lump)
!
      use t_finite_element_mat
!
      type(node_data), intent(in) :: node
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real(kind=kreal), intent(in)                                      &
     &         :: ff_smp(node%max_nod_smp,3,np_smp)
!
      type(lumped_mass_matrices), intent(inout) :: lump
!
!
      call cal_ff_smp_2_ml(node, rhs_tbl, lump%ml, lump%ml_o, ff_smp)
!
      end subroutine cal_ff_smp_2_ml_type
!
! ----------------------------------------------------------------------
!
      subroutine set_ff_nl_smp_2_ff(node, rhs_tbl, numdir)
!
      use m_finite_element_matrix
      use cal_rhs_node_order
!
      type(node_data), intent(in) :: node
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      integer(kind = kint), intent(in) :: numdir
!
!
      call s_set_ff_nl_smp_2_ff(node%numnod, np_smp,                    &
     &    node%max_nod_smp, node%istack_nod_smp,                        &
     &    rhs_tbl%node_sort_list_smp, numdir,                           &
     &    f1_l%ff_smp, ff_nl_smp, f1_l%ff, f1_nl%ff)
!
      end subroutine set_ff_nl_smp_2_ff
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_ff(node, rhs_tbl, numdir, ff_smp, ff)
!
      use cal_rhs_node_order
!
      type(node_data), intent(in) :: node
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      integer(kind = kint), intent(in) :: numdir
      real(kind = kreal), intent(in)                                    &
     &           :: ff_smp(node%max_nod_smp,3,np_smp)
!
      real(kind = kreal), intent(inout) :: ff(node%numnod,3)
!
!
      call s_cal_ff_smp_2_ff(node%numnod, np_smp, node%max_nod_smp,     &
     &    node%istack_nod_smp, rhs_tbl%node_sort_list_smp,              &
     &    numdir, ff_smp, ff)
!
      end subroutine cal_ff_smp_2_ff
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_ml(node, rhs_tbl, ml, ml_o, ff_smp)
!
      use cal_rhs_node_order
!
      type(node_data), intent(in) :: node
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real(kind=kreal), intent(in)                                      &
     &         :: ff_smp(node%max_nod_smp,3,np_smp)
      real(kind=kreal), intent(inout) :: ml(node%numnod)
      real(kind=kreal), intent(inout) :: ml_o(node%numnod)
!
!
      call s_cal_ff_smp_2_ml(node%numnod, np_smp, node%max_nod_smp,     &
     &    node%istack_nod_smp, rhs_tbl%node_sort_list_smp,              &
     &    ml, ml_o, ff_smp)
!
      end subroutine cal_ff_smp_2_ml
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_scalar                                    &
     &         (node, rhs_tbl, ff_smp, ml, ncomp_nod, i_fld, d_nod)
!
      use cal_rhs_node_order
!
      type(node_data), intent(in) :: node
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real(kind=kreal), intent(in) :: ml(node%numnod)
      real(kind=kreal), intent(in)                                      &
     &           :: ff_smp(node%max_nod_smp,3,np_smp)
!
      integer (kind=kint), intent(in) :: ncomp_nod, i_fld
      real(kind=kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
!
!
      call s_cal_ff_smp_2_scalar(node%numnod, np_smp,                   &
     &    node%max_nod_smp, node%istack_nod_smp,                        &
     &    rhs_tbl%node_sort_list_smp, d_nod(1,i_fld), ff_smp, ml)
!
      end subroutine cal_ff_smp_2_scalar
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_vector                                    &
     &         (node, rhs_tbl, ff_smp, ml, ncomp_nod, i_fld, d_nod)
!
      use m_phys_constants
      use cal_rhs_node_order
!
      type(node_data), intent(in) :: node
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real(kind=kreal), intent(in) :: ml(node%numnod)
      real(kind=kreal), intent(in)                                      &
     &                  :: ff_smp(node%max_nod_smp,n_vector,np_smp)
!
      integer(kind=kint), intent(in) :: ncomp_nod, i_fld
      real(kind=kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
!
!
      call s_cal_ff_smp_2_vector(node%numnod, np_smp,                   &
     &    node%max_nod_smp, node%istack_nod_smp,                        &
     &    rhs_tbl%node_sort_list_smp, d_nod(1,i_fld), ff_smp, ml)
!
      end subroutine cal_ff_smp_2_vector
!
! ----------------------------------------------------------------------
!
      subroutine cal_ff_smp_2_tensor                                    &
     &         (node, rhs_tbl, ff_smp, ml, ncomp_nod, i_fld, d_nod)
!
      use m_phys_constants
      use cal_rhs_node_order
!
      type(node_data), intent(in) :: node
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      real(kind=kreal), intent(in) :: ml(node%numnod)
      real(kind=kreal), intent(in)                                      &
     &            :: ff_smp(node%max_nod_smp,n_sym_tensor,np_smp)
!
      integer (kind=kint), intent(in) :: ncomp_nod, i_fld
      real(kind=kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
!
!
      call s_cal_ff_smp_2_tensor(node%numnod, np_smp,                   &
     &    node%max_nod_smp, node%istack_nod_smp,                        &
     &    rhs_tbl%node_sort_list_smp, d_nod(1,i_fld), ff_smp, ml)
!
      end subroutine cal_ff_smp_2_tensor
!
! ----------------------------------------------------------------------
!
      end module cal_ff_smp_to_ffs
