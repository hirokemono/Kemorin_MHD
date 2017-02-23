!
!     module sgs_ini_model_coefs_IO
!
!     programmed by H.Matsui in 2005
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine output_ini_model_coefs(cmt_param, wk_sgs, wk_diff)
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(dynamic_model_data), intent(in) :: wk_sgs
!!      subroutine input_ini_model_coefs                                &
!!     &         (cmt_param, ele, fluid, layer_tbl,                     &
!!     &          wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      module sgs_ini_model_coefs_IO
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_t_step_parameter
!
      use t_SGS_control_parameter
      use t_ele_info_4_dynamic
      use t_SGS_model_coefs
!
      implicit none
!
      integer (kind = kint) :: iflag_rst_sgs_coef_code
      integer (kind = kint), parameter :: rst_sgs_coef_code = 18
      character(len=kchara) :: rst_sgs_coef_head                        &
     &                       = 'rst_model_coefs'
      character(len=kchara) :: rst_sgs_coef_name                        &
     &                       = 'rst_model_coefs.dat'
!
      integer (kind = kint) :: n_layer_d_IO
      integer (kind = kint) :: num_sgs_kinds_IO, num_diff_kinds_IO
      character(len=kchara), allocatable :: name_ak_sgs_IO(:)
      character(len=kchara), allocatable :: name_ak_diff_IO(:)
!
      real(kind=kreal), allocatable :: coef_sgs_IO(:,:)
      real(kind=kreal), allocatable :: coef_diff_IO(:,:)
!
      private :: n_layer_d_IO, num_sgs_kinds_IO, num_diff_kinds_IO
      private :: name_ak_sgs_IO, name_ak_diff_IO
      private :: coef_sgs_IO, coef_diff_IO
      private :: read_ini_model_coefs
      private :: set_ini_model_coefs_from_IO
      private :: set_initial_model_coefs_ele
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine output_ini_model_coefs(cmt_param, wk_sgs, wk_diff)
!
      use open_sgs_model_coefs
      use sgs_model_coefs_IO
!
      type(commutation_control_params), intent(in) :: cmt_param
      type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!
      integer (kind = kint) :: inum
!
!
!
      if (my_rank.eq.0) then
!
        open (rst_sgs_coef_code,file = rst_sgs_coef_name)
!
!
        write(rst_sgs_coef_code,'(a)')                                  &
     &              '! time step and interbal for dynamic model'
        write(rst_sgs_coef_code,'(2i16)') i_step_MHD, i_step_sgs_coefs
!
        write(rst_sgs_coef_code,'(a)')  '! num. of model coefs'
        write(rst_sgs_coef_code,'(2i16)')                               &
     &       wk_sgs%num_kinds, wk_sgs%nlayer
!
        call write_sgs_coef_head(rst_sgs_coef_code, wk_sgs)
!
!   write model coefs for whole domain
        write(rst_sgs_coef_code,1000)  i_step_MHD, time, izero,         &
     &        wk_sgs%fld_whole_clip(1:wk_sgs%num_kinds)
!
!   write model coefs for each layer
        do inum = 1, wk_sgs%nlayer
          write(rst_sgs_coef_code,1000) i_step_MHD, time, inum,         &
     &       wk_sgs%fld_clip(inum,1:wk_sgs%num_kinds)
        end do
!
        if (cmt_param%iflag_commute .gt. id_SGS_commute_OFF) then
!
          write(rst_sgs_coef_code,'(a)')  '! num. of commute coefs'
          write(rst_sgs_coef_code,'(2i16)')                             &
     &       wk_diff%num_kinds, wk_diff%nlayer
!
          call write_diff_coef_head(rst_sgs_coef_code, wk_diff)
!
          write(rst_sgs_coef_code,1000) i_step_MHD, time, izero,        &
     &          wk_diff%fld_whole_clip(1:wk_diff%num_kinds)
!
          if (cmt_param%iset_DIFF_coefs .eq. 1 ) then
            do inum = 1, wk_diff%nlayer
              write(rst_sgs_coef_code,1000)  i_step_MHD, time, inum,    &
     &              wk_diff%fld_clip(inum,1:wk_diff%num_kinds)
            end do
          end if
        end if
        close (rst_sgs_coef_code)
!
      end if
!
 1000 format(i16,1pE25.15e3,i16,1p200E25.15e3)
!
      end subroutine output_ini_model_coefs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine input_ini_model_coefs                                  &
     &         (cmt_param, ele, fluid, layer_tbl,                       &
     &          wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      use t_geometry_data
      use t_geometry_data_MHD
      use t_layering_ele_list
!
      type(commutation_control_params), intent(in) :: cmt_param
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
      type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
!
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!
      call read_ini_model_coefs(cmt_param)
      call set_ini_model_coefs_from_IO(cmt_param, wk_sgs, wk_diff)
      call set_initial_model_coefs_ele                                  &
     &   (cmt_param, ele, fluid, layer_tbl%e_grp,                       &
     &    wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      end subroutine input_ini_model_coefs
!
!-----------------------------------------------------------------------
!
      subroutine read_ini_model_coefs(cmt_param)
!
      use skip_comment_f
      use sgs_model_coefs_IO
!
      type(commutation_control_params), intent(in) :: cmt_param
!
      character(len=255) :: character_4_read
      integer (kind = kint) :: itmp, i_step
!
!
      open (rst_sgs_coef_code,file = rst_sgs_coef_name,                 &
     &      status='old', ERR=99)
!
      call skip_comment(character_4_read, rst_sgs_coef_code)
      read(character_4_read,*) itmp, i_step_sgs_coefs
!
      call skip_comment(character_4_read, rst_sgs_coef_code)
      read(character_4_read,*) num_sgs_kinds_IO, n_layer_d_IO
!
      allocate( name_ak_sgs_IO(num_sgs_kinds_IO) )
      allocate( coef_sgs_IO(0:n_layer_d_IO,num_sgs_kinds_IO) )
      coef_sgs_IO = 0.0d0
!
      read(rst_sgs_coef_code,*) name_ak_sgs_IO(1:num_sgs_kinds_IO)
!
      call read_sgs_layerd_data(rst_sgs_coef_code, i_step,              &
     &    (n_layer_d_IO+1), num_sgs_kinds_IO, coef_sgs_IO(0,1) )
!
      if (cmt_param%iflag_commute .gt. id_SGS_commute_OFF) then
!
        call skip_comment(character_4_read, rst_sgs_coef_code)
        read(character_4_read,*) num_diff_kinds_IO, itmp
!
        allocate( name_ak_diff_IO(num_diff_kinds_IO) )
        read(rst_sgs_coef_code,*) name_ak_diff_IO(1:num_diff_kinds_IO)
!
        if (cmt_param%iset_DIFF_coefs .eq. 0) then
!
          allocate( coef_diff_IO(0:n_layer_d_IO,num_diff_kinds_IO) )
          coef_diff_IO = 0.0d0
!
          call read_sgs_layerd_data(rst_sgs_coef_code, i_step,         &
     &        (n_layer_d_IO+1), num_diff_kinds_IO, coef_diff_IO(0,1) )
!
        else
!
          allocate( coef_diff_IO(0:0,num_diff_kinds_IO) )
          coef_diff_IO = 0.0d0
!
          call read_sgs_layerd_data(rst_sgs_coef_code, i_step,         &
     &        ione, num_diff_kinds_IO, coef_diff_IO(0,1) )
!
        end if
      end if
!
      close(rst_sgs_coef_code)
      return
!
   99 continue
      num_sgs_kinds_IO = 0
      allocate( name_ak_sgs_IO(num_sgs_kinds_IO) )
      allocate( coef_sgs_IO(0:n_layer_d_IO,num_sgs_kinds_IO) )
      if (cmt_param%iflag_commute .gt. id_SGS_commute_OFF) then
        num_diff_kinds_IO = 0
        allocate( name_ak_diff_IO(num_diff_kinds_IO) )
        allocate( coef_diff_IO(0:n_layer_d_IO,num_diff_kinds_IO) )
      end if
      return
!
      end subroutine read_ini_model_coefs
!
!  ---------------------------------------------------------------------
!
      subroutine set_ini_model_coefs_from_IO                            &
     &         (cmt_param, wk_sgs, wk_diff)
!
      type(commutation_control_params), intent(in) :: cmt_param
      type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
!
      integer(kind = kint) :: i, j
!
!
      do i = 1, wk_sgs%num_kinds
        do j = 1, num_sgs_kinds_IO
          if ( wk_sgs%name(i) .eq. name_ak_sgs_IO(j) ) then
            wk_sgs%fld_clip(1:wk_sgs%nlayer,i)                          &
     &                 = coef_sgs_IO(1:wk_sgs%nlayer,j)
            wk_sgs%fld_whole_clip(i) =  coef_sgs_IO(0,j)
            exit
          end if
        end do
      end do
!
      deallocate(name_ak_sgs_IO)
      deallocate(coef_sgs_IO)
!
      if (cmt_param%iflag_commute .gt. id_SGS_commute_OFF) then
        do i = 1, wk_diff%num_kinds
          do j = 1, num_diff_kinds_IO
            if ( wk_diff%name(i) .eq. name_ak_diff_IO(j) ) then
              wk_diff%fld_whole_clip(i) = coef_diff_IO(0,j)
              if (cmt_param%iset_DIFF_coefs .eq. 1) then
                wk_diff%fld_clip(1:wk_diff%nlayer,i)                    &
     &                   = coef_diff_IO(1:wk_diff%nlayer,j)
              end if
              exit
            end if
          end do
        end do
!
        deallocate(name_ak_diff_IO)
        deallocate(coef_diff_IO)
!
      end if
!
      end subroutine set_ini_model_coefs_from_IO
!
!  ---------------------------------------------------------------------
!
      subroutine set_initial_model_coefs_ele                            &
     &         (cmt_param, ele, fluid, layer_egrp,                      &
     &          wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      use t_geometry_data
      use t_group_data
      use t_geometry_data_MHD
      use set_sgs_diff_model_coefs
!
      type(commutation_control_params), intent(in) :: cmt_param
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(group_data), intent(in) :: layer_egrp
      type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      integer(kind = kint) :: i, ist
!
      do i = 1, sgs_coefs%num_field
         ist = sgs_coefs%istack_comps(i-1) + 1
         call clear_model_coefs_2_ele(ele, sgs_coefs%num_comps(i), ist, &
     &      sgs_coefs%ntot_comp, sgs_coefs%ak)
         call set_model_coefs_2_ele(ele, izero, sgs_coefs%num_comps(i), &
     &       i, ist, layer_egrp%num_grp, layer_egrp%num_item,           &
     &       layer_egrp%istack_grp_smp, layer_egrp%item_grp,            &
     &       sgs_coefs%num_field, sgs_coefs%ntot_comp,                  &
     &       wk_sgs%fld_clip, wk_sgs%comp_clip, sgs_coefs%ak)
      end do
!
      if (cmt_param%iflag_commute .gt. id_SGS_commute_OFF) then
        if (cmt_param%iset_DIFF_coefs .eq. 0) then
          do i = 1, diff_coefs%num_field
            call set_diff_coefs_whole_ele                               &
     &         (ele, fluid%istack_ele_fld_smp, i, diff_coefs%ntot_comp, &
     &          wk_diff%fld_whole_clip, diff_coefs%ak)
          end do
        else
          do i = 1, diff_coefs%num_field
            call set_diff_coefs_layer_ele                               &
     &         (ele, i, layer_egrp%num_grp, layer_egrp%num_item,        &
     &          layer_egrp%istack_grp_smp, layer_egrp%item_grp,         &
     &          diff_coefs%ntot_comp, wk_diff%fld_clip, diff_coefs%ak)
          end do
        end if
      end if
!
      end subroutine set_initial_model_coefs_ele
!
!-----------------------------------------------------------------------
!
      end module sgs_ini_model_coefs_IO
