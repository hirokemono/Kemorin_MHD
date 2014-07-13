!>@file   single_assemble_sph.f90
!!@brief  module single_assemble_sph
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!
!!@verbatim
!!      subroutine init_para_assemble_sph
!!      subroutine analyze_para_assemble_sph
!!@endverbatim
!
      program assemble_newsph
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_phys_labels
      use m_merge_spheric_mesh
      use m_control_param_newsph
      use parallel_assemble_sph
      use copy_rj_phys_type_4_IO
!
!
      use m_control_data_4_merge
!
      use m_node_id_spherical_IO
      use merge_sph_step_spectr
      use field_IO_select
      use r_interpolate_marged_sph
!
      use m_sph_spectr_data
      use m_field_data_IO
      use m_t_step_parameter
      use merge_sph_step_spectr
      use field_IO_select
      use r_interpolate_marged_sph
      use copy_time_steps_4_restart
!
      implicit none
!
      type(rj_assemble_tbl), allocatable, save :: j_table_s(:,:)
!
      integer(kind = kint) :: istep
      integer(kind = kint) :: jp, ip, irank_org, num, k, j , inod, nd
!
!
      write(*,*) 'Simulation start: PE. '
!
      call read_control_assemble_sph
      call set_control_4_newsph
!
      call alloc_sph_mesh_4_merge
!
!  set original spectr data
!
      iflag_sph_file_fmt = ifmt_org_sph_file
      do ip = 1, np_sph_org
        irank_org = ip - 1
        call set_local_rj_mesh_4_merge(irank_org, org_sph_mesh(ip))
        call set_sph_boundary_4_merge(org_sph_mesh(ip)%sph_grps,        &
     &    nlayer_ICB_org, nlayer_CMB_org)
      end do
!
!  set new spectr data
!
      iflag_sph_file_fmt = ifmt_new_sph_file
      sph_file_head = new_sph_head
      do jp = 1, np_sph_new
        call set_local_rj_mesh_4_merge(jp-1, new_sph_mesh(jp))
        call set_sph_boundary_4_merge(new_sph_mesh(jp)%sph_grps,        &
     &      nlayer_ICB_new, nlayer_CMB_new)
      end do
!
!     Construct mode transfer table
!
      allocate(j_table_s(np_sph_new,np_sph_org))
      do jp = 1, np_sph_new
        do ip = 1, np_sph_org
          call alloc_each_mode_tbl_4_assemble                           &
     &      (org_sph_mesh(ip)%sph_mesh, j_table_s(jp,ip))
          call set_mode_table_4_assemble(org_sph_mesh(ip)%sph_mesh,     &
     &      new_sph_mesh(jp)%sph_mesh, j_table_s(jp,ip))
        end do
      end do
!
!     construct interpolation table
      call sph_radial_interpolation_coef                                &
     &     (org_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1),                 &
     &      org_sph_mesh(1)%sph_mesh%sph_rj%radius_1d_rj_r,             &
     &      new_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1),                 &
     &      new_sph_mesh(1)%sph_mesh%sph_rj%radius_1d_rj_r)
!
!      Construct field list from spectr file
!
        phys_file_head = org_sph_fst_head
        call sel_read_alloc_step_SPH_file(izero, istep_start)
        call copy_rj_phys_name_t_from_IO                                &
     &     (new_sph_mesh(1)%sph_mesh%sph_rj%nnod_rj, new_sph_phys(1))
        call deallocate_phys_data_IO
        call deallocate_phys_data_name_IO
!
      do jp = 2, np_sph_new
        new_sph_phys(jp)%num_phys =  new_sph_phys(1)%num_phys
        new_sph_phys(jp)%ntot_phys = new_sph_phys(1)%ntot_phys
!
        call alloc_phys_name_type(new_sph_phys(jp))
        call alloc_phys_data_type                                       &
     &     (new_sph_mesh(jp)%sph_mesh%sph_rj%nnod_rj, new_sph_phys(jp))
!
        new_sph_phys(jp)%num_component = new_sph_phys(1)%num_component
        new_sph_phys(jp)%istack_component                               &
     &           = new_sph_phys(1)%istack_component
        new_sph_phys(jp)%iflag_monitor = new_sph_phys(1)%iflag_monitor
        new_sph_phys(jp)%phys_name = new_sph_phys(1)%phys_name
      end do
!
      do ip = 1, np_sph_org
        org_sph_phys(ip)%num_phys =  new_sph_phys(1)%num_phys
        org_sph_phys(ip)%ntot_phys = new_sph_phys(1)%ntot_phys
        call alloc_phys_name_type(org_sph_phys(ip))
!
        org_sph_phys(ip)%num_component                                 &
     &       = new_sph_phys(1)%num_component
        org_sph_phys(ip)%istack_component                              &
     &       = new_sph_phys(1)%istack_component
        org_sph_phys(ip)%phys_name = new_sph_phys(1)%phys_name
      end do
!
!
!      do jp = 1, np_sph_new
!        do ip = 1, np_sph_org
!          do j = 1, org_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(2)
!            if(j_table_s(jp,ip)%j_org_to_new(j).gt. 0)               &
!     &        write(50+my_rank,*) ip, j,                               &
!     &        j_table_s(jp,ip)%j_org_to_new(j)
!          end do
!        end do
!      end do
!
! ----------------------------------------------------------------------
!
!     ---------------------
!
      do istep = istep_start, istep_end, increment_step
!
!     Load original spectr data
        phys_file_head = org_sph_fst_head
        do ip = 1, np_sph_org
          irank_org = ip - 1
          call sel_read_alloc_step_SPH_file(irank_org, istep)
!
          call copy_time_steps_from_restart
          call alloc_phys_data_type                                     &
     &     (org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj, org_sph_phys(ip))
          call copy_rj_phys_type_from_IO                                &
     &     (org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj, org_sph_phys(ip))
!
          call deallocate_phys_data_IO
          call deallocate_phys_data_name_IO
        end do
        time = time_init
        i_step_MHD = i_step_init
!
!     Bloadcast original spectr data
        do ip = 1, np_sph_org
          do jp = 1, np_sph_new
!
!     Copy spectr data to temporal array
            if(iflag_same_rgrid .eq. 0) then
              call r_itp_field_data_sph_assemble                        &
     &          (org_sph_mesh(ip)%sph_mesh, new_sph_mesh(jp)%sph_mesh,  &
     &           j_table_s(jp,ip), k_old2new_in, k_old2new_out,         &
     &           coef_old2new_in, kr_inner_domain, kr_outer_domain,     &
     &           new_sph_phys(jp)%ntot_phys, org_sph_phys(ip)%d_fld,    &
     &           new_sph_phys(jp)%d_fld)
            else
              call copy_field_data_sph_assemble&
     &          (org_sph_mesh(ip)%sph_mesh, new_sph_mesh(jp)%sph_mesh,  &
     &           j_table_s(jp,ip), new_sph_phys(jp)%ntot_phys,        &
     &           org_sph_phys(ip)%d_fld, new_sph_phys(jp)%d_fld)
            end if
!
            call copy_field_data_sph_center                             &
     &          (org_sph_mesh(ip)%sph_mesh, new_sph_mesh(jp)%sph_mesh,  &
     &           j_table_s(jp,ip), new_sph_phys(jp)%ntot_phys,          &
     &           org_sph_phys(ip)%d_fld, new_sph_phys(jp)%d_fld)
!
          end do
          call dealloc_phys_data_type(org_sph_phys(ip))
        end do
!
        do jp = 1, np_sph_new
          if(iflag_same_rgrid .eq. 0) then
!            write(*,*) 'extend_potential_magne'
            call extend_potential_magne_t                               &
     &         (new_sph_mesh(jp)%sph_mesh, new_sph_phys(jp))
!            write(*,*) 'extend_inner_core_temp'
            call extend_inner_core_scalar_t(fhd_temp,                   &
     &          new_sph_mesh(jp)%sph_mesh, new_sph_phys(jp))
!            write(*,*) 'extend_inner_core_light'
            call extend_inner_core_scalar_t(fhd_light,                  &
     &          new_sph_mesh(jp)%sph_mesh, new_sph_phys(jp))
          end if
!
          if(b_sph_ratio.ne.0.0d0 .or. b_sph_ratio.ne.1.0d0) then
            call mul_sph_magne(new_sph_mesh(jp)%sph_mesh%sph_rj%nnod_rj,&
     &        new_sph_phys(jp)%num_phys, new_sph_phys(jp)%ntot_phys,    &
     &        new_sph_phys(jp)%istack_component,                        &
     &        new_sph_phys(jp)%phys_name, new_sph_phys(jp)%d_fld)
          end if
!
!
          call copy_time_steps_to_restart
          call copy_rj_all_phys_name_t_to_IO                            &
     &     (new_sph_mesh(jp)%sph_mesh%sph_rj%nnod_rj, new_sph_phys(jp))
!
          phys_file_head = new_sph_fst_head
          numgrid_phys_IO = new_sph_mesh(jp)%sph_mesh%sph_rj%nnod_rj
          call allocate_phys_data_IO
!
          call copy_rj_all_phys_type_to_IO                              &
     &     (new_sph_mesh(jp)%sph_mesh%sph_rj%nnod_rj, new_sph_phys(jp))
!
          call sel_write_step_SPH_field_file(jp-1, istep)
          call deallocate_phys_data_IO
          call deallocate_phys_data_name_IO
        end do
!
        write(*,*) 'step', istep, 'finish '
      end do
!
      do jp = 1, np_sph_new
        do ip = 1, np_sph_org
          call dealloc_mode_table_4_assemble(j_table_s(jp,ip))
        end do
      end do
      deallocate(j_table_s)
!
      call dealloc_sph_mesh_4_merge
!
      end program assemble_newsph
