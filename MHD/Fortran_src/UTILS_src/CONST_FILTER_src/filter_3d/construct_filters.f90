!
!      module construct_filters
!
      module construct_filters
!
!     Written by H. Matsui on Mar., 2008
!
      use m_precision
!
      use m_constants
      use calypso_mpi
      use m_machine_parameter
      use m_ctl_params_4_gen_filter
      use m_geometry_parameter
      use m_reference_moments
      use m_element_id_4_node
      use m_next_node_id_4_node
      use cal_element_size
!
      use cal_filter_moms_ele_by_elen
      use expand_filter_area_4_1node
!
      implicit none
!
      character(len=kchara), parameter, private :: tmp_head = 'work'
!
!      subroutine const_commutative_filter
!      subroutine const_simple_filter
!      subroutine correct_commutative_filter
!      subroutine correct_by_simple_filter
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_commutative_filter
!
      use m_filter_elength
      use m_filter_moments
      use cal_filter_func_node
!
!
      if(iflag_debug.eq.1)  write(*,*)'cal_fmoms_ele_by_elen_1st'
      call allocate_filter_moms_nod(FEM1_elen%nnod_filter_mom)
      call allocate_filter_moms_ele(FEM1_elen%nele_filter_mom)
      call cal_fmoms_ele_by_elen_1st(ione)
      call cal_fmoms_ele_by_elen_1st(itwo)
!
      if (itype_mass_matrix .eq. 1) call release_mass_mat_for_consist
!
!  ---------------------------------------------------
!     construct filter function
!  ---------------------------------------------------
!
      if(iflag_debug.eq.1)  write(*,*) 'const_commute_filter_coefs'
      call const_commute_filter_coefs
!
      call deallocate_iele_belonged
      call deallocate_inod_next_node
!
      if(iflag_debug.eq.1)  write(*,*)'const_fluid_filter_coefs'
      call const_fluid_filter_coefs
!
      call finalize_4_cal_fileters
!
      call deallocate_iele_belonged
      call deallocate_inod_next_node
!
      call deallocate_coef_4_filter_moms
!
      end subroutine const_commutative_filter
!
! ----------------------------------------------------------------------
!
      subroutine const_simple_filter
!
      use m_element_id_4_node
      use m_next_node_id_4_node
      use m_finite_element_matrix
      use m_filter_elength
      use m_filter_moments
      use cal_1st_diff_deltax_4_nod
      use cal_filter_func_node
      use cal_diff_elesize_on_ele
      use filter_geometry_IO
!
!
      if(iflag_debug.eq.1) write(*,*) 'allocate_filter_moms_nod'
      call allocate_filter_moms_nod(FEM1_elen%nnod_filter_mom)
      if(iflag_debug.eq.1) write(*,*) 'allocate_filter_moms_ele'
      call allocate_filter_moms_ele(FEM1_elen%nele_filter_mom)
!
!  ---------------------------------------------------
!     construct filter function
!  ---------------------------------------------------
!
        if(iflag_debug.eq.1) write(*,*) 'set_simple_filter'
        call set_simple_filter
!
        call deallocate_iele_belonged
        call deallocate_inod_next_node
!
          if(iflag_debug.eq.1)  write(*,*) 's_const_filter_mom_ele 1'
        call s_const_filter_mom_ele(ione)
          if(iflag_debug.eq.1)                                          &
     &       write(*,*) 'cal_fmoms_ele_by_elen_1st 2'
        call cal_fmoms_ele_by_elen_1st(itwo)
!
          if(iflag_debug.eq.1)  write(*,*) 'set_simple_fluid_filter'
        call set_simple_fluid_filter
!
        call cal_fmoms_ele_by_elen_1st(itwo)
        if (itype_mass_matrix .eq. 1) call release_mass_mat_for_consist
!
        call deallocate_iele_belonged
        call deallocate_inod_next_node
!
!        if(iflag_debug.eq.1)  write(*,*) 'finalize_4_cal_fileters'
!        call finalize_4_cal_fileters
!
      end subroutine const_simple_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine correct_commutative_filter
!
      use m_filter_file_names
      use m_filter_elength
      use m_filter_moments
      use set_parallel_file_name
      use correct_wrong_filters
      use filter_geometry_IO
!
      character(len=kchara) :: file_name
!
!
      if(iflag_debug.eq.1)  write(*,*)'cal_fmoms_ele_by_elen_1st'
      call allocate_filter_moms_nod(FEM1_elen%nnod_filter_mom)
      call allocate_filter_moms_ele(FEM1_elen%nele_filter_mom)
      call cal_fmoms_ele_by_elen_1st(ione)
      call cal_fmoms_ele_by_elen_1st(itwo)
      if (itype_mass_matrix .eq. 1) call release_mass_mat_for_consist
!
!  ---------------------------------------------------
!     check filter function
!  ---------------------------------------------------
!
      call add_int_suffix(my_rank, org_filter_coef_head,                &
     &        file_name)
      if (ifmt_3d_filter .eq. iflag_ascii) then
        open(org_filter_coef_code, file=file_name, form='formatted')
        call read_filter_geometry(org_filter_coef_code)
      else
        open(org_filter_coef_code, file=file_name, form='unformatted')
        call read_filter_geometry_b(org_filter_coef_code)
      end if
!
      call deallocate_node_data_dummy
      call deallocate_comm_item_IO
!
!  ---------------------------------------------------
!     correct filter function
!  ---------------------------------------------------
!
      call allocate_correct_filter_flag(numnod, numele)
!
      if(iflag_debug.eq.1)  write(*,*) 's_correct_wrong_filters'
      call s_correct_wrong_filters(org_filter_coef_code)
!
      call deallocate_iele_belonged
      call deallocate_inod_next_node
!
      if(iflag_debug.eq.1)  write(*,*)'correct_wrong_fluid_filters'
      call correct_wrong_fluid_filters(org_filter_coef_code)
!
      call deallocate_correct_filter_flag
!
      call deallocate_iele_belonged
      call deallocate_inod_next_node
!
      call deallocate_coef_4_filter_moms
      call finalize_4_cal_fileters
!
      close(org_filter_coef_code)
!
      end subroutine correct_commutative_filter
!
! ----------------------------------------------------------------------
!
      subroutine correct_by_simple_filter
!
      use m_filter_file_names
      use m_filter_elength
      use m_filter_moments
      use m_filter_coefs
      use m_field_file_format
      use set_parallel_file_name
      use correct_wrong_filters
      use filter_geometry_IO
!
      character(len=kchara) :: file_name
!
!
          if(iflag_debug.eq.1) write(*,*) 'allocate_filter_moms_nod'
      call allocate_filter_moms_nod(FEM1_elen%nnod_filter_mom)
          if(iflag_debug.eq.1) write(*,*) 'allocate_filter_moms_ele'
      call allocate_filter_moms_ele(FEM1_elen%nele_filter_mom)
!
!  ---------------------------------------------------
!     check filter function
!  ---------------------------------------------------
!
      write(*,*) 'org_filter_coef_head', org_filter_coef_head
      call add_int_suffix(my_rank, org_filter_coef_head, file_name)
      if (ifmt_3d_filter .eq. iflag_ascii) then
        open(org_filter_coef_code, file=file_name, form='formatted')
        call read_filter_geometry(org_filter_coef_code)
      else
        open(org_filter_coef_code, file=file_name, form='unformatted')
        call read_filter_geometry_b(org_filter_coef_code)
      end if
!
      call deallocate_node_data_dummy
      call deallocate_comm_item_IO
!
!  ---------------------------------------------------
!     correct filter function
!  ---------------------------------------------------
!
      call allocate_correct_filter_flag(numnod, numele)
!
      if(iflag_debug.eq.1)  write(*,*) 's_correct_wrong_filters'
      call s_correct_wrong_filters(org_filter_coef_code)
!
      call deallocate_iele_belonged
      call deallocate_inod_next_node
!
        if(iflag_debug.eq.1)  write(*,*) 's_const_filter_mom_ele 1'
      call s_const_filter_mom_ele(ione)
        if(iflag_debug.eq.1)                                            &
     &       write(*,*) 'correct_filter_moms_ele_by_elen 1'
      call correct_fmoms_ele_by_elen_1st(ione)
        if(iflag_debug.eq.1)                                            &
     &       write(*,*) 'cal_fmoms_ele_by_elen_1st 2'
      call cal_fmoms_ele_by_elen_1st(itwo)
!
      if(iflag_debug.eq.1)  write(*,*)'correct_wrong_fluid_filters'
      call correct_wrong_fluid_filters(org_filter_coef_code)
!
      call deallocate_correct_filter_flag
!
      call deallocate_iele_belonged
      call deallocate_inod_next_node
!
      call deallocate_coef_4_filter_moms
      call finalize_4_cal_fileters
!
      close(org_filter_coef_code)
!
      call cal_fmoms_ele_by_elen_1st(itwo)
!
      end subroutine correct_by_simple_filter
!
! ----------------------------------------------------------------------
!
      end module construct_filters
