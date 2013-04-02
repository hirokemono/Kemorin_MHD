!read_layer_evo_file_header.f90
!
!      module read_layer_evo_file_header
!
!        Written by H. Matsui on March, 2012
!
!      subroutine count_num_comp_evo_file(id_file, file_name,           &
!     &          num_extra, num_comp)
!      subroutine count_num_comp_layer_evo_file(id_file, file_name,     &
!     &          num_extra, num_comp, num_layer)
!
!      subroutine read_field_name_evo_file(id_file, file_name,          &
!     &          num_extra, num_comp, field_name)
!      subroutine read_evolution_data(id_file, num_comp, num_layer,     &
!     &          istep, time, d_grp, ierr)
!
      module read_layer_evo_file_header
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine count_num_comp_evo_file(id_file, file_name,            &
     &          num_extra, num_comp)
!
      use skip_comment_f
!
      character(len=kchara), intent(in)  ::  file_name
      integer(kind = kint), intent(in) :: id_file, num_extra
      integer(kind = kint), intent(inout) :: num_comp
!
      integer(kind = kint), parameter :: lengthbuf = 8192
      character(len=lengthbuf) :: charabuf
      character(len=kchara)  ::    field_name(255)
!
!
      open (id_file, file=file_name)
      call count_field_by_comma(id_file, charabuf,                      &
     &    num_comp, field_name)
      close(id_file)
      num_comp = num_comp - num_extra
!
      end subroutine count_num_comp_evo_file
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_num_comp_layer_evo_file(id_file, file_name,      &
     &          num_extra, num_comp, num_layer)
!
      use skip_comment_f
!
      character(len=kchara), intent(in)  ::  file_name
      integer(kind = kint), intent(in) :: id_file, num_extra
!
      integer(kind = kint), intent(inout) :: num_comp
      integer(kind = kint), intent(inout) :: num_layer
!
      integer(kind = kint) :: istep, i_layer, istep_org
      real(kind = kreal) :: time
!
      integer(kind = kint), parameter :: lengthbuf = 8192
      character(len=lengthbuf) :: charabuf
      character(len=kchara)  ::    field_name(255)
!
!
      open (id_file,file=file_name)
!
      call count_field_by_comma(id_file, charabuf,                      &
     &    num_comp, field_name)
      num_comp = num_comp - num_extra
!
      read(charabuf,*) istep_org, time, i_layer
      num_layer = i_layer
      do
        read(id_file,*,err=99, end=99) istep, time, i_layer
        if (istep .ne. istep_org) exit
        istep_org = istep
        num_layer = i_layer
      end do
  99  continue
      close(id_file)
!
      end subroutine count_num_comp_layer_evo_file
!
!   --------------------------------------------------------------------
!
      subroutine read_field_name_evo_file(id_file, file_name,           &
     &          num_extra, num_comp, field_name)
!
      character(len=kchara), intent(in)  ::  file_name
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: num_comp, num_extra
      character(len=kchara), intent(inout) ::  field_name(num_comp)
!
      integer(kind = kint) :: k
      character(len=kchara)  :: ctmp
!
!
      open (id_file, file=file_name)
      read(id_file,*)  (ctmp,k=1,num_extra), field_name(1:num_comp)
!
      end subroutine read_field_name_evo_file
!
!   --------------------------------------------------------------------
!
      subroutine read_evolution_data(id_file, num_comp, num_layer,      &
     &          istep, time, d_grp, ierr)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: num_comp, num_layer
!
      integer(kind = kint), intent(inout) :: istep, ierr
      real(kind = kreal), intent(inout) :: time
      real(kind = kreal), intent(inout) :: d_grp(num_layer,num_comp)
      integer(kind = kint) :: itmp
      integer(kind = kint) :: i
!
!
      ierr = 0
      do i = 1, num_layer
        read(id_file,*,err=99,end=99) istep, time, itmp,                &
     &        d_grp(i,1:num_comp)
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_evolution_data
!
!   --------------------------------------------------------------------
!
      end module read_layer_evo_file_header
