!>@file   t_spectr_data_4_assemble.f90
!!@brief  module t_spectr_data_4_assemble
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!@endverbatim
!
      module t_spectr_data_4_assemble
!
      use m_precision
      use t_SPH_mesh_field_data
      use t_phys_data
      use t_field_data_IO
      use t_time_data
      use parallel_assemble_sph
!
      implicit none
!
!
      type spectr_data_4_assemble
        integer(kind = kint) :: np_sph_org
        type(sph_mesh_data), allocatable :: org_sph_mesh(:)
        type(phys_data), allocatable ::     org_sph_phys(:)
!
        integer(kind = kint) :: np_sph_new
        type(sph_mesh_data), allocatable :: new_sph_mesh(:)
        type(phys_data), allocatable ::     new_sph_phys(:)
!
        type(rj_assemble_tbl), allocatable :: j_table(:,:)
!
!
        integer(kind = kint) :: nloop_new
        type(field_IO), allocatable :: new_fst_IO(:)
        type(time_data) :: fst_time_IO
!
        integer(kind = kint), allocatable :: nnod_list_lc(:)
        integer(kind = kint), allocatable :: nnod_list(:)
        integer(kind = kint_gl), allocatable :: istack_nnod_list(:)
!
!
        integer(kind = kint) :: nlayer_ICB_org
        integer(kind = kint) :: nlayer_CMB_org
!
        integer(kind = kint) :: nlayer_ICB_new
        integer(kind = kint) :: nlayer_CMB_new
        type(sph_radial_itp_data) :: r_itp
      end type spectr_data_4_assemble
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_spectr_data_4_assemble(sph_asbl)
!
      type(spectr_data_4_assemble), intent(inout) :: sph_asbl
!
!
      allocate( sph_asbl%org_sph_mesh(sph_asbl%np_sph_org) )
      allocate( sph_asbl%org_sph_phys(sph_asbl%np_sph_org) )
      allocate( sph_asbl%new_sph_mesh(sph_asbl%np_sph_new) )
      allocate( sph_asbl%new_sph_phys(sph_asbl%np_sph_new) )
      allocate(sph_asbl%j_table(sph_asbl%np_sph_org,                    &
     &                          sph_asbl%np_sph_new))
!
      end subroutine alloc_spectr_data_4_assemble
!
! ----------------------------------------------------------------------
!
      subroutine alloc_spectr_list_4_assemble(sph_asbl)
!
      type(spectr_data_4_assemble), intent(inout) :: sph_asbl
!
!
      allocate(sph_asbl%nnod_list_lc(sph_asbl%np_sph_new))
      allocate(sph_asbl%nnod_list(sph_asbl%np_sph_new))
      allocate(sph_asbl%istack_nnod_list(0:sph_asbl%np_sph_new))
!
      sph_asbl%nnod_list_lc(1:sph_asbl%np_sph_new) =     0
      sph_asbl%nnod_list(1:sph_asbl%np_sph_new) =        0
      sph_asbl%istack_nnod_list(0:sph_asbl%np_sph_new) = 0
!
      end subroutine alloc_spectr_list_4_assemble
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_spectr_data_4_assemble(nprocs, sph_asbl)
!
      integer(kind = kint), intent(in) :: nprocs
      type(spectr_data_4_assemble), intent(inout) :: sph_asbl
!
      integer(kind = kint) :: ip, jp
!
!
      do jp = 1, sph_asbl%np_sph_new
        if(mod(jp-1,nprocs) .ne. my_rank) cycle
        do ip = 1, sph_asbl%np_sph_org
          call dealloc_mode_table_4_assemble(sph_asbl%j_table(ip,jp))
        end do
      end do
      deallocate(sph_asbl%j_table)
!
      deallocate(sph_asbl%org_sph_mesh, sph_asbl%org_sph_phys)
      deallocate(sph_asbl%new_sph_mesh, sph_asbl%new_sph_phys)
!
      end subroutine dealloc_spectr_data_4_assemble
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_spectr_list_4_assemble(sph_asbl)
!
      type(spectr_data_4_assemble), intent(inout) :: sph_asbl
!
!
      deallocate(sph_asbl%istack_nnod_list)
      deallocate(sph_asbl%nnod_list)
      deallocate(sph_asbl%nnod_list_lc)
!
      end subroutine dealloc_spectr_list_4_assemble
!
! ----------------------------------------------------------------------
!
      module t_spectr_data_4_assemble
