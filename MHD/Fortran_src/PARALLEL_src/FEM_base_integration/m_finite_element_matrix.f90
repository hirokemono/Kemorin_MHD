!
!     module m_finite_element_matrix
!.......................................................................
!
!     Written by H. Matsui and H. Okuda on Jul. 2000
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine allocate_finite_elem_mt
!
!      subroutine allocate_fem_mat_base
!
!      subroutine reset_ff_smp
!      subroutine reset_ff_smps
!      subroutine reset_ff(numnod)
!      subroutine reset_ff_t_smp
!
!      subroutine deallocate_fem_mat_base
!      subroutine deallocate_fem_mat_region
!      subroutine deallocate_fem_mat_fluid
!      subroutine deallocate_node_ff
!
      module m_finite_element_matrix
!
      use m_precision
      use t_finite_element_mat
!
      implicit  none
!
!>      Work area for FEM assemble
      type(work_finite_element_mat), save :: fem1_wk
!fem1_wk%me_diag
!
      real (kind=kreal), allocatable  ::  ml(:)
      real (kind=kreal), allocatable  ::  ml_fl(:)
      real (kind=kreal), allocatable  ::  ml_cd(:)
      real (kind=kreal), allocatable  ::  ml_ins(:)
!
!      real (kind=kreal), allocatable  ::  ml_ele_diag(:)
!
      real (kind=kreal), allocatable  ::  ml_o(:)
      real (kind=kreal), allocatable  ::  ml_o_fl(:)
      real (kind=kreal), allocatable  ::  ml_o_cd(:)
      real (kind=kreal), allocatable  ::  ml_o_ins(:)
!
!
      real (kind=kreal), allocatable  ::  ff(:,:)
      real (kind=kreal), allocatable  ::  ff_nl(:,:)
      real (kind=kreal), allocatable  ::  ff_t(:,:)
! 
      real (kind=kreal), allocatable  :: ff_smp(:,:,:)
      real (kind=kreal), allocatable  :: ff_nl_smp(:,:,:)
! 
      real (kind=kreal), allocatable  :: ff_m_smp(:,:,:)
      real (kind=kreal), allocatable  :: ff_t_smp(:,:,:)
!
!      real (kind=kreal), allocatable  ::  sk6(:,:,:)
!
      private :: allocate_fem_mat_region
      private :: allocate_fem_mat_fluid, allocate_node_ff
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine allocate_finite_elem_mt
!
      use m_geometry_data
!
!
      call allocate_fem_mat_base
      call allocate_fem_mat_region(node1%numnod)
!
      end subroutine allocate_finite_elem_mt
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine allocate_fem_mat_base
!
      use m_geometry_data
      use m_machine_parameter
      use m_phys_constants
!
!
      call allocate_node_ff(node1%numnod)
!
      allocate(ml(node1%numnod))
      allocate(ml_o(node1%numnod))
!
      allocate(ff_smp(node1%max_nod_smp,3,np_smp))
      allocate(ff_nl_smp(node1%max_nod_smp,3,np_smp))
      allocate(ff_m_smp(node1%max_nod_smp,3,np_smp))
      allocate(ff_t_smp(node1%max_nod_smp,6,np_smp))
!
      allocate(fem1_wk%sk6(ele1%numele,n_sym_tensor,ele1%nnod_4_ele))
!
      allocate(fem1_wk%scalar_1(ele1%numele) )
      allocate(fem1_wk%vector_1(ele1%numele,3) )
      allocate(fem1_wk%tensor_1(ele1%numele,6) )
!
      allocate(fem1_wk%me_diag(ele1%numele))
!
      if(node1%numnod .gt. 0) then
        ml =  0.0d0
        ml_o =0.0d0
      end if
!
      if(node1%max_nod_smp .gt. 0) then
        ff_m_smp = 0.0d0
        ff_t_smp = 0.0d0
      end if
!
      if(ele1%numele .gt. 0) then
        fem1_wk%sk6 =      0.0d0
        fem1_wk%scalar_1 = 0.0d0
        fem1_wk%vector_1 = 0.0d0
        fem1_wk%tensor_1 = 0.0d0
!
        fem1_wk%me_diag = 0.0d0
      end if
!
      call reset_ff_smps
!
      end subroutine allocate_fem_mat_base
!
!   ---------------------------------------------------------------------
!
      subroutine allocate_fem_mat_region(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
!
      call allocate_fem_mat_fluid(numnod)
!
      allocate(ml_cd(numnod))
      allocate(ml_ins(numnod))
!
      allocate(ml_o_cd(numnod))
      allocate(ml_o_ins(numnod))
!
      if(numnod .le. 0) return
      ml_cd =  0.0d0
      ml_ins = 0.0d0
!
      ml_o_cd =  0.0d0
      ml_o_ins = 0.0d0
!
      end subroutine allocate_fem_mat_region
!
!   ---------------------------------------------------------------------
!
      subroutine allocate_fem_mat_fluid(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
!
      allocate(ml_fl(numnod))
      allocate(ml_o_fl(numnod))
!
      if(numnod .le. 0) return
      ml_fl =   0.0d0
      ml_o_fl = 0.0d0
!
      end subroutine allocate_fem_mat_fluid
!
!   ---------------------------------------------------------------------
!
      subroutine allocate_node_ff(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
!
      allocate(ff(numnod,3))
      allocate(ff_nl(numnod,3))
      allocate(ff_t(numnod,6))
!
      if(numnod .le. 0) return
      call reset_ff(numnod)
      ff_nl = 0.0d0
      ff_t =  0.0d0
!
      end subroutine allocate_node_ff
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine reset_ff_smp
!
      use m_machine_parameter
      use m_geometry_data
!
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 1, np_smp
        ff_smp(1:node1%max_nod_smp,1:3,ip) =   0.0d0
      end do
!$omp end parallel do
!
      end subroutine reset_ff_smp
!
!   ---------------------------------------------------------------------
!
      subroutine reset_ff_smps
!
      use m_machine_parameter
      use m_geometry_data
!
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 1, np_smp
        ff_smp(1:node1%max_nod_smp,1:3,ip) =   0.0d0
        ff_nl_smp(1:node1%max_nod_smp,1:3,ip) = 0.0d0
      end do
!$omp end parallel do
!
      end subroutine reset_ff_smps
!
!   ---------------------------------------------------------------------
!
      subroutine reset_ff(numnod)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint) :: inod, nd
!
!
!$omp parallel private(inod)
      do nd = 1, 3
!$omp do
        do inod = 1, numnod
          ff(inod,nd) = 0.0d0
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine reset_ff
!
!   ---------------------------------------------------------------------
!
      subroutine reset_ff_t_smp
!
      use m_machine_parameter
      use m_geometry_data
!
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 1, np_smp
        ff_t_smp(1:node1%max_nod_smp,1:6,ip) =   0.0d0
      end do
!$omp end parallel do
!
      end subroutine reset_ff_t_smp
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine deallocate_fem_mat_base
!
      deallocate(ml, ml_o)
!
      deallocate(ff_smp, ff_nl_smp, ff_m_smp, ff_t_smp)
      deallocate(fem1_wk%sk6)
      deallocate(fem1_wk%scalar_1, fem1_wk%vector_1, fem1_wk%tensor_1)
!
      deallocate(fem1_wk%me_diag)
!
      call deallocate_node_ff
!
      end subroutine deallocate_fem_mat_base
!
!   ---------------------------------------------------------------------
!
      subroutine deallocate_fem_mat_region
!
      deallocate(ml_cd,  ml_o_cd)
      deallocate(ml_ins, ml_o_ins)
!
      call deallocate_fem_mat_fluid
!
      end subroutine deallocate_fem_mat_region
!
!   ---------------------------------------------------------------------
!
      subroutine deallocate_fem_mat_fluid
!
      deallocate(ml_fl, ml_o_fl)
!
      end subroutine deallocate_fem_mat_fluid
!
!   ---------------------------------------------------------------------
!
      subroutine deallocate_node_ff
!
      deallocate(ff, ff_nl, ff_t)
!
      end subroutine deallocate_node_ff
!
!   ---------------------------------------------------------------------
!
      end module m_finite_element_matrix
