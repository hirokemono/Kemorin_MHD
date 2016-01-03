!
!     module sgs_ini_model_coefs_IO
!
!     programmed by H.Matsui in 2005
!     modified by H. Matsui on Aug., 2007
!
!      subroutine output_ini_model_coefs
!      subroutine input_ini_model_coefs(layer_tbl)
!        type(layering_tbl), intent(in) :: layer_tbl
!
      module sgs_ini_model_coefs_IO
!
      use m_precision
!
      use m_constants
      use calypso_mpi
      use m_control_parameter
      use m_t_step_parameter
      use m_geometry_data
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
      private :: set_initial_model_coefs_ele
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine output_ini_model_coefs
!
      use m_SGS_model_coefs
      use open_sgs_model_coefs
      use sgs_model_coefs_IO
!
      integer (kind = kint) :: inum, nd
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
        write(rst_sgs_coef_code,'(2i16)')  num_sgs_kinds, nlayer_SGS
!
        call write_sgs_coef_head(rst_sgs_coef_code)
!
!   write model coefs for whole domain
        write(rst_sgs_coef_code,1000)  i_step_MHD, time, izero,         &
     &        (sgs_f_whole_clip(nd),nd=1,num_sgs_kinds)
!
!   write model coefs for each layer
        do inum = 1, nlayer_SGS
          write(rst_sgs_coef_code,1000) i_step_MHD, time, inum,         &
     &     (sgs_f_clip(inum,nd),nd=1,num_sgs_kinds)
        end do
!
        if (iflag_commute_correction .gt. id_SGS_commute_OFF) then
!
          write(rst_sgs_coef_code,'(a)')  '! num. of commute coefs'
          write(rst_sgs_coef_code,'(2i16)')  num_diff_kinds, nlayer_SGS
!
          call write_diff_coef_head(rst_sgs_coef_code)
!
          write(rst_sgs_coef_code,1000) i_step_MHD, time, izero,        &
     &          (diff_f_whole_clip(nd),nd=1, num_diff_kinds)
!
          if (iset_DIFF_model_coefs .eq. 1 ) then
            do inum = 1, nlayer_SGS
              write(rst_sgs_coef_code,1000)  i_step_MHD, time, inum,    &
     &              (diff_f_clip(inum,nd),nd=1,num_diff_kinds)
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
      subroutine input_ini_model_coefs(layer_tbl)
!
      use t_layering_ele_list
!
      type(layering_tbl), intent(in) :: layer_tbl
!
!
      call read_ini_model_coefs
      call set_ini_model_coefs_from_IO
      call set_initial_model_coefs_ele(layer_tbl%e_grp)
!
      end subroutine input_ini_model_coefs
!
!-----------------------------------------------------------------------
!
      subroutine read_ini_model_coefs
!
      use skip_comment_f
      use sgs_model_coefs_IO
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
      if (iflag_commute_correction .gt. id_SGS_commute_OFF) then
!
        call skip_comment(character_4_read, rst_sgs_coef_code)
        read(character_4_read,*) num_diff_kinds_IO, itmp
!
        allocate( name_ak_diff_IO(num_diff_kinds_IO) )
        read(rst_sgs_coef_code,*) name_ak_diff_IO(1:num_diff_kinds_IO)
!
        if (iset_DIFF_model_coefs .eq. 0) then
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
      if (iflag_commute_correction .gt. id_SGS_commute_OFF) then
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
      subroutine set_ini_model_coefs_from_IO
!
      use m_SGS_model_coefs
      use m_ele_info_4_dynamical
!
      integer(kind = kint) :: i, j
!
!
      do i = 1, num_sgs_kinds
        do j = 1, num_sgs_kinds_IO
          if ( name_ak_sgs(i) .eq. name_ak_sgs_IO(j) ) then
            sgs_f_clip(1:nlayer_SGS,i) = coef_sgs_IO(1:nlayer_SGS,j)
            sgs_f_whole_clip(i) =           coef_sgs_IO(0,j)
            exit
          end if
        end do
      end do
!
      deallocate(name_ak_sgs_IO)
      deallocate(coef_sgs_IO)
!
      if (iflag_commute_correction .gt. id_SGS_commute_OFF) then
        do i = 1, num_diff_kinds
          do j = 1, num_diff_kinds_IO
            if ( name_ak_diff(i) .eq. name_ak_diff_IO(j) ) then
              diff_f_whole_clip(i) = coef_diff_IO(0,j)
              if (iset_DIFF_model_coefs .eq. 1) then
                diff_f_clip(1:nlayer_SGS,i)                             &
     &                   = coef_diff_IO(1:nlayer_SGS,j)
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
      subroutine set_initial_model_coefs_ele(layer_egrp)
!
      use t_group_data
      use m_SGS_model_coefs
      use m_geometry_data_MHD
      use set_sgs_diff_model_coefs
!
      type(group_data), intent(in) :: layer_egrp
!
      integer(kind = kint) :: i, ist
!
      do i = 1, num_sgs_kinds
         ist = istack_sgs_coefs(i-1) + 1
         call set_model_coefs_2_ele(izero, ncomp_sgs_coefs(i), i, ist,  &
     &       layer_egrp%num_grp, layer_egrp%num_item,                   &
     &       layer_egrp%istack_grp_smp, layer_egrp%item_grp, ele1)
      end do
!
      if (iflag_commute_correction .gt. id_SGS_commute_OFF) then
        if (iset_DIFF_model_coefs .eq. 0) then
          do i = 1, num_diff_kinds
            call set_diff_coefs_whole_ele                               &
     &         (fluid1%istack_ele_fld_smp, i, ele1)
          end do
        else
          do i = 1, num_diff_kinds
            call set_diff_coefs_layer_ele                               &
     &         (i, layer_egrp%num_grp, layer_egrp%num_item,             &
     &          layer_egrp%istack_grp_smp, layer_egrp%item_grp, ele1)
          end do
        end if
      end if
!
      end subroutine set_initial_model_coefs_ele
!
!-----------------------------------------------------------------------
!
      end module sgs_ini_model_coefs_IO
