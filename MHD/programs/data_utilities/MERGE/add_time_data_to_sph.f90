!add_time_data_to_sph.f90
! ----- program  add_time_data_to_sph
!
      program add_time_data_to_sph
!
!    change restart data for different number of domains
!     By H. Matsui
!
!
      use m_precision
!
      use m_spheric_parameter
!
      use m_control_data_4_merge
      use m_control_param_newsph
      use m_merge_spheric_mesh
      use merge_sph_step_spectr
      use set_field_file_names
!
      implicit    none
!
      integer (kind = kint) :: istep
!
! ==============================================
! * get number of  nodes,elements for whole PES
! ==============================================
!
!
      call read_control_assemble_sph
      call set_control_4_newsph
!
      call alloc_sph_mesh_4_merge
!
!  set spectr data
!
      call set_sph_rj_mesh_4_merge
!
!   loop for time integration
!
      do istep = istep_start, istep_end, increment_step
        call add_time_step_4_sph_file(istep)
        write(*,*) 'step', istep, 'finish '
      end do
!
      if(iflag_delete_org_sph .gt. 0) then
        do istep = istep_start, istep_end, increment_step
          phys_file_head = org_sph_fst_head
          call delete_SPH_fld_file(iflag_field_data_fmt,                &
     &        np_sph_org, istep)
        end do
      end if
!
      call deallocate_sph_1d_index_rj
      call deallocate_spheric_param_rj
!
!
      stop ' //// program normally finished //// '
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine add_time_step_4_sph_file(istep)
!
      use input_old_file_sel_4_zlib
      use field_IO_select
      use copy_rj_phys_data_4_IO
!
      integer(kind = kint), intent(in) :: istep
!
      integer(kind = kint) :: ip, my_rank
!
!
      phys_file_head = org_sph_fst_head
      do ip = 1, np_sph_org
        my_rank = ip - 1
        call sel_read_alloc_field_file(my_rank, istep)
        call deallocate_phys_data_name_IO
!
        call copy_rj_merged_phys_from_IO                                &
     &      (org_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj(2),               &
     &       org_sph_mesh(ip)%sph_mesh%sph_rj%idx_gl_1d_rj_j)
!
        call deallocate_phys_data_IO
      end do
!
      if(b_sph_ratio.ne.0.0d0 .or. b_sph_ratio.ne.1.0d0) then
        call mul_sph_magne
      end if
!
!  set new spectr data
!
      call copy_rj_all_phys_name_to_IO
!
      phys_file_head = new_sph_fst_head
      do ip = 1, np_sph_new
        my_rank = ip - 1
!
        numgrid_phys_IO = new_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj
        call allocate_phys_data_IO
!
        call copy_rj_merged_phys_to_IO                                  &
     &      (new_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj(2),               &
     &       new_sph_mesh(ip)%sph_mesh%sph_rj%idx_gl_1d_rj_j)
!
        call sel_write_step_SPH_field_file(my_rank, istep)
        call deallocate_phys_data_IO
      end do
!
      call deallocate_phys_data_name_IO
!
      end subroutine add_time_step_4_sph_file
!
! -------------------------------------------------------------------
!
      end program add_time_data_to_sph
