!const_layering_table.f90
!     module const_layering_table
!
!      Written by H.Matsui
!      Moified by H. Matsui on Sep., 2007
!      Moified by H. Matsui on Feb., 2008
!
!      subroutine marking_by_layers(numele, num_mat, num_mat_bc,        &
!     &          mat_istack, mat_name, mat_item, mat_flag_mhd)
!
!.......................................................................
!
!     subroutine for re-orderinf of elements
!      by insulator, conductor, and fluid
!
!.......................................................................
!
!    insulated solid:   mat_flag_mhd = 0
!    Conductive solid:  mat_flag_mhd = 1
!    insulated fluid:   mat_flag_mhd = 2
!    Conductive fluid:  mat_flag_mhd = 3
!    
!    Ordering of elemets
!       Conductive solid:  mat_flag_mhd = 1
!       Conductive fluid:  mat_flag_mhd = 3
!       insulated fluid:   mat_flag_mhd = 2
!       insulated solid:   mat_flag_mhd = 0
!       Do nothing:        mat_flag_mhd < 0
!
!.......................................................................
!
!      subroutine const_table_by_layers(numele, mat_flag_mhd,           &
!     &          iele_fl_st, iele_cd_st, iele_ins_st,                   &
!     &          iele_fl_ed, iele_cd_ed, iele_ins_ed,                   &
!     &          new2oldele_layer, old2newele_layer)
!
!.......................................................................
!
!     subroutine for re-orderinf of elements
!      by insulator, conductor, and fluid
!
!.......................................................................
!
      module const_layering_table
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine marking_by_layers(numele, num_mat, num_mat_bc,         &
     &          mat_istack, mat_name, mat_item, mat_flag_mhd)
!
      use m_control_parameter
      use skip_comment_f
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: num_mat, num_mat_bc
      integer (kind = kint), intent(in) :: mat_istack(0:num_mat)
      integer (kind = kint), intent(in) :: mat_item(num_mat_bc)
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer (kind = kint), intent(inout) :: mat_flag_mhd(numele)
!
      integer (kind = kint) :: iflag
      integer (kind = kint) :: j, j2
      integer (kind = kint) :: iele, inum
!
!
      do iele = 1, numele
        mat_flag_mhd(iele) = 0
      end do
!
!  ----   conductive  layer .... all
!
      if(cmp_no_case(FEM_prm1%condutive_group%group_name(1),'all')) then
        do iele = 1, numele
          mat_flag_mhd(iele) = 1
        end do
!
!  ----   insulated  layer .... all
!
       else if(cmp_no_case(FEM_prm1%condutive_group%group_name(1),'none')) &
      & then
         do iele = 1, numele
           mat_flag_mhd(iele) = 0
         end do
!
!    conductor and insulator exist
!
      else
!
        do j = 1, num_mat
          iflag = 0
!
          do j2 = 1, num_ins_ele_grp
            if ( mat_name(j) .eq. ins_ele_grp_name(j2) ) then
              iflag = 0
              exit
            end if
          end do
!
          do j2 = 1, FEM_prm1%condutive_group%num_group
            if(mat_name(j) .eq. FEM_prm1%condutive_group%group_name(j2)) &
      &      then
              iflag = 1
              do inum = mat_istack(j-1)+1, mat_istack(j)
                iele = mat_item(inum)
                mat_flag_mhd(iele) = iflag
              end do
              exit
            end if
          end do
!
        end do
!
      end if
!
!  ----   fluid layer .... all
!
      if (cmp_no_case(FEM_prm1%fluid_group%group_name(1),'all')) then
        do iele = 1, numele
          mat_flag_mhd(iele) = mat_flag_mhd(iele) + 2
        end do
!
!  ------  entire solid
!
      else if(cmp_no_case(FEM_prm1%fluid_group%group_name(1),'none'))   &
     & then
        do iele = 1, numele
          mat_flag_mhd(iele) = mat_flag_mhd(iele)
        end do
!
! -------- mixtured
!
      else
!
        do j = 1, num_mat
          iflag = 0
!
          do j2 = 1, FEM_prm1%fluid_group%num_group
            if (mat_name(j) .eq. FEM_prm1%fluid_group%group_name(j2))   &
     &       then
              iflag = 2
            end if
          end do
!
          do inum = mat_istack(j-1)+1, mat_istack(j)
            iele = mat_item(inum)
            mat_flag_mhd(iele) = mat_flag_mhd(iele) + iflag
          end do
!
        end do
!
      end if
!
      end subroutine marking_by_layers
!
! -----------------------------------------------------------------------
!
      subroutine const_table_by_layers(numele, mat_flag_mhd,            &
     &          iele_fl_st, iele_cd_st, iele_ins_st,                    &
     &          iele_fl_ed, iele_cd_ed, iele_ins_ed,                    &
     &          new2oldele_layer, old2newele_layer)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: mat_flag_mhd(numele)
!
      integer (kind = kint), intent(inout) :: iele_fl_st
      integer (kind = kint), intent(inout) :: iele_cd_st
      integer (kind = kint), intent(inout) :: iele_ins_st
      integer (kind = kint), intent(inout) :: iele_fl_ed
      integer (kind = kint), intent(inout) :: iele_cd_ed
      integer (kind = kint), intent(inout) :: iele_ins_ed
      integer (kind = kint), intent(inout) :: new2oldele_layer(numele)
      integer (kind = kint), intent(inout) :: old2newele_layer(numele)
!
      integer (kind = kint) :: iflag
      integer (kind = kint) :: iele, iele0, inum
!
!
!  set list vector for ordering
!
!
      iele = 0
      do inum = 0, 3
!
       if ( inum.eq. 0 ) then
         iflag = 1
         iele_cd_st = iele + 1
       else if ( inum.eq. 1 ) then
         iflag = 3
         iele_fl_st = iele + 1
       else if ( inum.eq. 2 ) then
         iflag = 2
         iele_ins_st = iele + 1
       else if ( inum.eq. 3 ) then
         iflag = 0
       end if
!
       do iele0 = 1, numele
!
        if ( mat_flag_mhd(iele0) .eq. iflag ) then
          iele = iele + 1
          new2oldele_layer(iele) = iele0
        end if
!
       end do
!
       if ( inum.eq. 1 ) then
         iele_cd_ed = iele
       else if ( inum.eq. 2 ) then
         iele_fl_ed = iele
       else if ( inum.eq. 3 ) then
         iele_ins_ed = iele
       end if
!
      end do
!
!
      do iele = 1, numele
        inum = new2oldele_layer(iele)
        old2newele_layer(inum) = iele
      end do
!
!
      end subroutine const_table_by_layers
!
! -----------------------------------------------------------------------
!
      end module const_layering_table
