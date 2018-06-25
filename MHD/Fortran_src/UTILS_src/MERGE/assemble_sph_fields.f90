!>@file   assemble_sph_fields.f90
!!@brief  module assemble_sph_fields
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2014
!
!>@brief routines for parallel spectr data assemble
!!
!!@verbatim
!!      subroutine share_org_sph_rj_data(np_sph_org, org_sph_mesh)
!!        type(sph_mesh_data), intent(inout) :: org_sph_mesh(np_sph_org)
!!      subroutine share_spectr_field_names(np_sph_org, np_sph_new,     &
!!     &          new_sph_mesh, org_sph_phys, new_sph_phys)
!!
!!      subroutine load_new_spectr_rj_data(np_sph_org, np_sph_new,      &
!!     &          org_sph_mesh, new_sph_mesh, j_table)
!!        type(sph_mesh_data), intent(in) :: org_sph_mesh(np_sph_org)
!!        type(sph_mesh_data), intent(in) :: new_sph_mesh(np_sph_new)
!!        type(rj_assemble_tbl), intent(inout)                          &
!!       &                             :: j_table(np_sph_org,np_sph_new)
!!@endverbatim
!!
!
      module assemble_sph_fields
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_sph_spectr_data
      use t_SPH_mesh_field_data
!
      implicit none
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine share_org_sph_rj_data(np_sph_org, org_sph_mesh)
!
      use share_spectr_index_data
!
      integer(kind = kint), intent(in) :: np_sph_org
      type(sph_mesh_data), intent(inout) :: org_sph_mesh(np_sph_org)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_sph_org
        call share_sph_rj_data(ip, org_sph_mesh(ip))
      end do
!
      end subroutine share_org_sph_rj_data
!
! -----------------------------------------------------------------------
!
      subroutine share_spectr_field_names(np_sph_org, np_sph_new,       &
     &          new_sph_mesh, org_sph_phys, new_sph_phys)
!
      use share_field_data
!
      integer(kind = kint), intent(in) :: np_sph_org, np_sph_new
      type(sph_mesh_data), intent(in) :: new_sph_mesh(np_sph_new)
      type(phys_data), intent(inout) :: org_sph_phys(np_sph_org)
      type(phys_data), intent(inout) :: new_sph_phys(np_sph_new)
!
      integer(kind = kint) :: ip, jp
!
!
      do ip = 1, np_sph_org
        call share_phys_field_names(org_sph_phys(ip))
      end do
!
!
      call share_phys_field_names(new_sph_phys(1))
!
      do jp = 2, np_sph_new
        if(mod(jp-1,nprocs) .ne. my_rank) cycle
        call copy_field_name_type(new_sph_phys(1), new_sph_phys(jp))
      end do
!
      do jp = 1, np_sph_new
        if(mod(jp-1,nprocs) .ne. my_rank) cycle
         call alloc_phys_data_type                                      &
     &     (new_sph_mesh(jp)%sph%sph_rj%nnod_rj, new_sph_phys(jp))
      end do
!
      end subroutine share_spectr_field_names
!
! -----------------------------------------------------------------------
!
      subroutine load_new_spectr_rj_data(np_sph_org, np_sph_new,        &
     &          org_sph_mesh, new_sph_mesh, j_table)
!
      use parallel_assemble_sph
      use new_SPH_restart
!
      integer(kind = kint), intent(in) :: np_sph_org, np_sph_new
      type(sph_mesh_data), intent(in) :: org_sph_mesh(np_sph_org)
      type(sph_mesh_data), intent(in) :: new_sph_mesh(np_sph_new)
      type(rj_assemble_tbl), intent(inout)                              &
     &                             :: j_table(np_sph_org,np_sph_new)
!
      integer(kind = kint) :: iproc, jp, jproc, jrank_new
!
!     Construct mode transfer table
      do jp = 0, (np_sph_new-1) / nprocs
        jrank_new = my_rank + jp * nprocs
        jproc = jrank_new + 1
        if(jrank_new .ge. np_sph_new) cycle
        do iproc = 1, np_sph_org
          call alloc_each_mode_tbl_4_assemble                           &
     &      (org_sph_mesh(iproc)%sph, j_table(iproc,jproc))
          call set_mode_table_4_assemble(org_sph_mesh(iproc)%sph,       &
     &        new_sph_mesh(jproc)%sph, j_table(iproc,jproc))
        end do
      end do
!
      end subroutine load_new_spectr_rj_data
!
! -----------------------------------------------------------------------
!
      end module assemble_sph_fields
