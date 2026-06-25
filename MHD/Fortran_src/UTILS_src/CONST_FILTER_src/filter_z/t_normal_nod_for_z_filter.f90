!>@file   t_normal_nod_for_z_filter.f90
!!        module t_normal_nod_for_z_filter
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief FEM shape functgions for vertical filter
!!
!!@verbatim
!!      subroutine alloc_normal_nod_z_filter                            &
!!     &         (node, nfilter2_3, numfilter, nrm_z_fil)
!!        subroutine dealloc_normal_nod_z_filter(nrm_z_fil)
!!        type(node_data), intent(in) :: node
!!        integer(kind = kint), intent(in) :: nfilter2_3, numfilter
!!        type(normal_nod_for_z_filter), intent(inout) :: nrm_z_fil
!!
!!      subroutine check_nod_normalize_matrix(id_rank, node, nrm_z_fil)
!!      subroutine check_integrated_values(id_file, nrm_z_fil)
!!        integer, intent(in) :: id_rank
!!        type(node_data), intent(in) :: node
!!        type(normal_nod_for_z_filter), intent(inout) :: nrm_z_fil
!!@endverbatim
!
      module t_normal_nod_for_z_filter
!
      use m_precision
!
      use t_geometry_data
      use t_edge_data
      use t_gauss_points
!
      implicit none
!
      type normal_nod_for_z_filter
        integer(kind = kint) :: nfilter6_1 = 0
        real(kind = kreal), allocatable :: sk_norm_n(:)
        real(kind = kreal), allocatable :: f_mom_full(:)
!
        integer(kind = kint) :: nfilter2_3 = 0
        real(kind = kreal), allocatable:: d_norm_nod(:,:,:)
      end type normal_nod_for_z_filter
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_normal_nod_z_filter                              &
     &         (node, nfilter2_3, numfilter, nrm_z_fil)
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: nfilter2_3, numfilter
      type(normal_nod_for_z_filter), intent(inout) :: nrm_z_fil
!
!
      nrm_z_fil%nfilter2_3 = nfilter2_3
      nrm_z_fil%nfilter6_1 = 6*numfilter + 1
!
      allocate(nrm_z_fil%sk_norm_n(0:nrm_z_fil%nfilter6_1))
      allocate(nrm_z_fil%f_mom_full(0:nrm_z_fil%nfilter6_1))
!
      nrm_z_fil%sk_norm_n(0:nrm_z_fil%nfilter6_1) =  0.0d0
      nrm_z_fil%f_mom_full(0:nrm_z_fil%nfilter6_1) = 0.0d0
!
      allocate(nrm_z_fil%d_norm_nod(node%numnod,                        &
     &                              nfilter2_3,0:nfilter2_3))
      nrm_z_fil%d_norm_nod(1:node%numnod,1:nfilter2_3,0:nfilter2_3)     &
     &                                                         = 0.0d0
!
      end subroutine alloc_normal_nod_z_filter
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_normal_nod_z_filter(nrm_z_fil)
!
      type(normal_nod_for_z_filter), intent(inout) :: nrm_z_fil
!
      deallocate(nrm_z_fil%sk_norm_n)
      deallocate(nrm_z_fil%d_norm_nod)
!
      end subroutine dealloc_normal_nod_z_filter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine check_nod_normalize_matrix(id_rank, node, nrm_z_fil)
!
      integer, intent(in) :: id_rank
      type(node_data), intent(in) :: node
      type(normal_nod_for_z_filter), intent(inout) :: nrm_z_fil
!
      integer(kind = kint) :: i, k
!
!
      do k = 0, nrm_z_fil%nfilter2_3
        do i = 1, node%numnod
        write(id_rank+60,*) 'd_norm_nod (node_id,order) = ', i, k
        write(id_rank+60,'(1p5e16.8)')                                  &
     &          nrm_z_fil%d_norm_nod(i,1:nrm_z_fil%nfilter2_3,k)
        end do
      end do
!
      end subroutine check_nod_normalize_matrix
!
! -----------------------------------------------------------------------
!
      subroutine check_integrated_values(id_file, nrm_z_fil)
!
      integer, intent(in) :: id_file
      type(normal_nod_for_z_filter), intent(in) :: nrm_z_fil
!
      write(id_file,*) 'f_mom_full'
      write(id_file,'(1p5e16.8)')                                       &
     &                    nrm_z_fil%f_mom_full(0:nrm_z_fil%nfilter6_1)
!
      end subroutine check_integrated_values
!
!  ---------------------------------------------------------------------
!
      end module t_normal_nod_for_z_filter
