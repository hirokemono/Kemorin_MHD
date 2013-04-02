!write_djds_mat_MHD.f90
!     module   write_djds_mat_MHD
!
!     Written by H. Matsui on Apr., 2008
!
!      subroutine s_write_djds_mat_MHD
!
!      subroutine write_djds_mat_velo
!      subroutine write_djds_mat_press
!      subroutine write_djds_mat_magne
!      subroutine write_djds_mat_mag_p
!      subroutine write_djds_mat_temp
!      subroutine write_djds_mat_d_scalar
!
      module   write_djds_mat_MHD
!
      use m_precision
!
      use m_machine_parameter
      use m_parallel_var_dof
      use m_iccg_parameter
      use m_geometry_parameter
!
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
      use write_djds_matrix_data
      use write_djds_matrix_struct
      use set_parallel_file_name
!
      implicit  none
!
      integer(kind = kint), parameter :: id_mat_file = 14
      character(len=kchara) :: fhead_velo_mat =    'djds_mat_velo'
      character(len=kchara) :: fhead_press_mat =   'djds_mat_press'
      character(len=kchara) :: fhead_magne_mat =   'djds_mat_magne'
      character(len=kchara) :: fhead_magp_mat =    'djds_mat_magp'
      character(len=kchara) :: fhead_temp_mat =    'djds_mat_temp'
      character(len=kchara) :: fhead_dscalar_mat = 'djds_mat_scalar'
!
      character(len=kchara) :: fname, fname_tmp
!
      private ::  fname, fname_tmp, id_mat_file
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_write_djds_mat_MHD
!
      use m_control_parameter
!
!
      if ( iflag_t_evo_4_velo .ge. 1 ) then
        call write_djds_mat_press
      end if
!
      if ( iflag_t_evo_4_velo .ge. 3 ) then
        call write_djds_mat_velo
      end if
!
      if ( iflag_t_evo_4_temp.ge.3 ) then
        call write_djds_mat_temp
      end if
!
      if ( iflag_t_evo_4_composit.ge.3 ) then
        call write_djds_mat_d_scalar
      end if
!
      if (iflag_t_evo_4_vect_p.ge.1 .or. iflag_t_evo_4_magne.ge.1) then
        call write_djds_mat_mag_p
      end if
!
      if (iflag_t_evo_4_vect_p.ge.1 .or. iflag_t_evo_4_magne.ge.1) then
        call write_djds_mat_magne
      end if
!
      end subroutine s_write_djds_mat_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat_velo
!
      use m_comm_table_4_MHD
      use m_solver_djds_fluid
      use m_velo_matrix
!
      integer(kind = kint) :: i
!
!
      if ( ((METHOD(1:1).eq.'M').or.(METHOD(1:1).eq.'m')) .and.         &
     &     ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) .and.         &
     &     ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.         &
     &     ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) ) then
        do i = 0, num_MG_level
          call add_int_suffix(i, fhead_velo_mat, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname)
          open(id_mat_file, file=fname)
          call write_djds_mat33_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl_fl(i), MG_mat_velo(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm_fl(i), MG_djds_tbl_fl(i), MG_mat_velo(i) )
          close(id_mat_file)
        end do
      else
        call add_int_suffix(my_rank, fhead_velo_mat, fname)
        open(id_mat_file, file=fname)
        call write_djds_mat33_comp                                      &
     &     (id_mat_file, internal_node, numnod, NLmax, NUmax,           &
     &     itotal_fl_l, itotal_fl_u, (NLmax*np_smp), (NUmax*np_smp),    &
     &     NHYP, np_smp, NEWtoOLD, aiccg_velo(im_velo_d),               &
     &     indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,            &
     &     aiccg_velo(im_velo_l), aiccg_velo(im_velo_u),                &
     &     ALUG_velo_L, ALUG_velo_U)
        call write_djds_mat_connects                                    &
     &     (id_mat_file, numnod, np_smp, NHYP, inter_smp_stack,         &
     &     STACKmc, NLmaxHYP, NUmaxHYP, IVECT,                          &
     &     OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, NEWtoOLD_DJDS_U, LtoU,     &
     &     neigh_pe_num_fl, istack_export_fl, NOD_EXPORT_NEW_fl)
        close(id_mat_file)
      end if
!
      end subroutine write_djds_mat_velo
!
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat_press
!
      use m_comm_table_4_MHD
      use m_solver_djds_linear_fl
      use m_press_matrix
!
      integer(kind = kint) :: i
!
!
      if ( ((METHOD(1:1).eq.'M').or.(METHOD(1:1).eq.'m')) .and.         &
     &     ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) .and.         &
     &     ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.         &
     &     ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) ) then
        do i = 0, num_MG_level
          call add_int_suffix(i, fhead_press_mat, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname)
          open(id_mat_file, file=fname)
          call write_djds_mat11_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl_fll(i), MG_mat_press(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm_fl(i), MG_djds_tbl_fll(i), MG_mat_press(i) )
          close(id_mat_file)
        end do
      else
        call add_int_suffix(my_rank, fhead_press_mat, fname)
        open(id_mat_file, file=fname)
        call write_djds_mat11_comp                                      &
     &    (id_mat_file, internal_node, numnod, NLmax1, NUmax1,          &
     &    itotal1_fl_l, itotal1_fl_u, (NLmax1*np_smp), (NUmax1*np_smp), &
     &    NHYP1, np_smp, NEWtoOLD1, aiccg_press(im_press_d),            &
     &    indexDJDS1_L, indexDJDS1_U, itemDJDS1_L, itemDJDS1_U,         &
     &    aiccg_press(im_press_l), aiccg_press(im_press_u),             &
     &    ALUG_press_L, ALUG_press_U)
        call write_djds_mat_connects                                    &
     &     (id_mat_file, numnod, np_smp, NHYP1, inter_smp_stack,        &
     &     STACKmc1, NLmaxHYP1, NUmaxHYP1, IVECT1,                      &
     &     OLDtoNEW_DJDS1_L, OLDtoNEW_DJDS1_U, NEWtoOLD_DJDS1_U, LtoU1, &
     &     neigh_pe_num_fl, istack_export_fl, NOD_EXPORT_NEW_fl1)
        close(id_mat_file)
      end if
!
      end subroutine write_djds_mat_press
!
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat_magne
!
      use m_nod_comm_table
      use m_solver_djds
      use m_magne_matrix
!
      integer(kind = kint) :: i
!
!
      if ( ((METHOD(1:1).eq.'M').or.(METHOD(1:1).eq.'m')) .and.         &
     &     ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) .and.         &
     &     ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.         &
     &     ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) ) then
        do i = 0, num_MG_level
          call add_int_suffix(i, fhead_magne_mat, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname)
          open(id_mat_file, file=fname)
          call write_djds_mat33_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl(i), MG_mat_magne(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm(i), MG_djds_tbl_fl(i), MG_mat_magne(i) )
          close(id_mat_file)
        end do
      else
        call add_int_suffix(my_rank, fhead_magne_mat, fname)
        open(id_mat_file, file=fname)
        call write_djds_mat33_comp                                      &
     &     (id_mat_file, internal_node, numnod, NLmax, NUmax,           &
     &     itotal_l, itotal_u, (NLmax*np_smp), (NUmax*np_smp),          &
     &     NHYP, np_smp, NEWtoOLD, aiccg_magne(im_mag_d),               &
     &     indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,            &
     &     aiccg_magne(im_mag_l), aiccg_magne(im_mag_u),                &
     &     ALUG_magne_L, ALUG_magne_U)
        call write_djds_mat_connects                                    &
     &     (id_mat_file, numnod, np_smp, NHYP, inter_smp_stack,         &
     &     STACKmc, NLmaxHYP, NUmaxHYP, IVECT,                          &
     &     OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, NEWtoOLD_DJDS_U, LtoU,     &
     &     num_neib, istack_export, NOD_EXPORT_NEW)
        close(id_mat_file)
      end if
!
      end subroutine write_djds_mat_magne
!
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat_mag_p
!
      use m_nod_comm_table
      use m_solver_djds_linear
      use m_mag_potential_matrix
!
      integer(kind = kint) :: i
!
!
      if ( ((METHOD(1:1).eq.'M').or.(METHOD(1:1).eq.'m')) .and.         &
     &     ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) .and.         &
     &     ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.         &
     &     ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) ) then
        do i = 0, num_MG_level
          call add_int_suffix(i, fhead_magp_mat, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname)
          open(id_mat_file, file=fname)
          call write_djds_mat11_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl_l(i), MG_mat_magp(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm(i), MG_djds_tbl_l(i), MG_mat_magp(i) )
          close(id_mat_file)
        end do
      else
        call add_int_suffix(my_rank, fhead_magp_mat, fname)
        open(id_mat_file, file=fname)
        call write_djds_mat11_comp                                      &
     &     (id_mat_file, internal_node, numnod, NLmax1, NUmax1,         &
     &     itotal1_l, itotal1_u, (NLmax1*np_smp), (NUmax1*np_smp),      &
     &      NHYP1, np_smp, NEWtoOLD1, aiccg_mag_p(im_mp_d),             &
     &     indexDJDS1_L, indexDJDS1_U, itemDJDS1_L, itemDJDS1_U,        &
     &     aiccg_mag_p(im_mp_l), aiccg_mag_p(im_mp_u),                  &
     &     ALUG_mag_p_L, ALUG_mag_p_U)
        call write_djds_mat_connects                                    &
     &     (id_mat_file, numnod, np_smp, NHYP1, inter_smp_stack,        &
     &     STACKmc1, NLmaxHYP1, NUmaxHYP1, IVECT1,                      &
     &     OLDtoNEW_DJDS1_L, OLDtoNEW_DJDS1_U, NEWtoOLD_DJDS1_U, LtoU1, &
     &     num_neib, istack_export, NOD_EXPORT_NEW1)
        close(id_mat_file)
      end if
!
      end subroutine write_djds_mat_mag_p
!
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat_temp
!
      use m_comm_table_4_MHD
      use m_solver_djds_fluid
      use m_temp_matrix
!
      integer(kind = kint) :: i
!
!
      if ( ((METHOD(1:1).eq.'M').or.(METHOD(1:1).eq.'m')) .and.         &
     &     ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) .and.         &
     &     ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.         &
     &     ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) ) then
        do i = 0, num_MG_level
          call add_int_suffix(i, fhead_temp_mat, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname)
          open(id_mat_file, file=fname)
          call write_djds_mat11_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl_fl(i), MG_mat_temp(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm_fl(i), MG_djds_tbl_fl(i), MG_mat_temp(i) )
          close(id_mat_file)
        end do
      else
        call add_int_suffix(my_rank, fhead_temp_mat, fname)
        open(id_mat_file, file=fname)
        call write_djds_mat11_comp                                      &
     &     (id_mat_file, internal_node, numnod, NLmax, NUmax,           &
     &     itotal_fl_l, itotal_fl_u, (NLmax*np_smp), (NUmax*np_smp),    &
     &     NHYP, np_smp, NEWtoOLD, aiccg_temp(im_temp_d),               &
     &     indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,            &
     &     aiccg_temp(im_temp_l), aiccg_temp(im_temp_u),                &
     &     ALUG_temp_l, ALUG_temp_u)
        call write_djds_mat_connects                                    &
     &     (id_mat_file, numnod, np_smp, NHYP, inter_smp_stack,         &
     &     STACKmc, NLmaxHYP, NUmaxHYP, IVECT,                          &
     &     OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, NEWtoOLD_DJDS_U, LtoU,     &
     &     neigh_pe_num_fl, istack_export_fl, NOD_EXPORT_NEW_fl)
        close(id_mat_file)
      end if
!
      end subroutine write_djds_mat_temp
!
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat_d_scalar
!
      use m_comm_table_4_MHD
      use m_solver_djds_fluid
      use m_light_element_matrix
!
      integer(kind = kint) :: i
!
!
      if ( ((METHOD(1:1).eq.'M').or.(METHOD(1:1).eq.'m')) .and.         &
     &     ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) .and.         &
     &     ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.         &
     &     ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) ) then
        do i = 0, num_MG_level
          call add_int_suffix(i, fhead_dscalar_mat, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname)
          open(id_mat_file, file=fname)
          call write_djds_mat11_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl_fl(i), MG_mat_d_scalar(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm_fl(i), MG_djds_tbl_fl(i), MG_mat_d_scalar(i) )
          close(id_mat_file)
        end do
      else
        call add_int_suffix(my_rank, fhead_dscalar_mat, fname)
        open(id_mat_file, file=fname)
        call write_djds_mat11_comp                                      &
     &     (id_mat_file, internal_node, numnod, NLmax, NUmax,           &
     &     itotal_fl_l, itotal_fl_u, (NLmax*np_smp), (NUmax*np_smp),    &
     &     NHYP, np_smp,NEWtoOLD, aiccg_composit(im_cps_d),             &
     &     indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,            &
     &     aiccg_composit(im_cps_l), aiccg_composit(im_cps_u),          &
     &     ALUG_composit_l, ALUG_composit_u)
        call write_djds_mat_connects                                    &
     &     (id_mat_file, numnod, np_smp, NHYP, inter_smp_stack,         &
     &     STACKmc, NLmaxHYP, NUmaxHYP, IVECT,                          &
     &     OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, NEWtoOLD_DJDS_U, LtoU,     &
     &     neigh_pe_num_fl, istack_export_fl, NOD_EXPORT_NEW_fl)
        close(id_mat_file)
      end if
!
      end subroutine write_djds_mat_d_scalar
!
! ----------------------------------------------------------------------
!
      end module write_djds_mat_MHD
